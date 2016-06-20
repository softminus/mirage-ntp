open History
open Wire
open Maybe

type counter = Cstruct.uint64 [@printer fun fmt -> fprintf fmt "0x%Lx"]
[@@deriving show]

type regime = ZERO | WARMUP | NORMAL (* turns out we *do* need to distinguish between ZERO and WARMUP *)
[@@deriving show]

type physical_parameters = {
    skm_scale:          float;
    ts_limit:           float;
    skm_rate:           float;
    e_offset:           float;
    e_offset_qual:      float;

    shift_thres:        counter;    (* rtt shift detection *)

    point_error_thresh:     float;
    rate_error_threshold:   float;
    rate_sanity:            float;

    local_rate_error_threshold: float;
    local_rate_sanity:          float;

    initial_p:         (float * float);
}
[@@deriving show]

type nonce = {
    tsc:    counter;    (* the TSC value when we send it *)
    txts:   ts;         (* the random number that was in the transmit_timestamp of the packet we sent *)
}

type timestamps = {
    ta:     counter;
    tb:     float               [@printer fun fmt -> fprintf fmt "%.09f"];
    te:     float               [@printer fun fmt -> fprintf fmt "%.09f"];
    tf:     counter;
}
[@@deriving show]

type quality = NG | OK
[@@deriving show]

type sample = {
    quality:    quality;
    ttl:        int;
    stratum:    Wire.stratum;
    leap:       Wire.leap;
    refid:      Cstruct.uint32  [@printer fun fmt -> fprintf fmt "0x%lx"];
    rootdelay:  float;
    rootdisp:   float;
    timestamps: timestamps;
}
[@@deriving show]

type estimators = {
    pstamp:                          point  option; (* this is a point within samples, not rtt_hat *)
    p_hat_and_error:        (float * float) option;
    p_local:                (float * float) option;
    c:                               float  option;
    theta_hat_and_error:    (float * float * (sample * counter)) option;
}
[@@deriving show]

type sync_state = {
    regime:                 regime;
    parameters:             physical_parameters;
    samples_and_rtt_hat:   (sample * counter) history;  (* sample history and RTT_hat history are zipped together *)
    estimators:             estimators;
}
[@@deriving show]

type output = {
    skm_scale:                      float;
    freshness:                      counter;
    p_hat_and_error:        (float * float);
    p_local:                (float * float) option;
    ca_and_error:           (float * float);
}
[@@deriving show]





let default_parameters =
    let skm_scale           = 1024.0 in
    let ts_limit            = 1.5e-5 in
    let skm_rate            = 2e-7 in
    let rate_error_bound    = 5.0e-7 in

    let initial_p       = (1e-9, 0.0) in            (* FIXME: set this to the right period based on nominal counter frequency *)

    let e_offset        = 6.0 *. ts_limit in (* 6 = offset_ratio *)
    let e_offset_qual   = 3.0 *. e_offset in
    let shift_thres     = 0L in                     (* FIXME *)

    let point_error_thresh = 3.0 *. ts_limit in
    let rate_error_threshold    = rate_error_bound /. 5.0 in
    let rate_sanity             = rate_error_bound *. 3.0 in

    let local_rate_sanity           = rate_error_bound *. 3.0   in
    let local_rate_error_threshold  = 8.0e-7                    in
    {skm_scale; ts_limit; skm_rate; e_offset; e_offset_qual; initial_p; shift_thres; point_error_thresh; rate_error_threshold; rate_sanity; local_rate_sanity; local_rate_error_threshold}

let delta_TSC newer older =
    let del = Int64.sub newer older in
    match (del >= 0L) with
    | true  -> del
    | false -> failwith "invalid ΔTSC!"

let baseline newer older =
    delta_TSC (fst newer).timestamps.tf (fst older).timestamps.tf

let rtt_of_prime sample =
    let ts = sample.timestamps in
    let del = Int64.sub (ts.tf) (ts.ta) in
    match (del > 0L) with
    | true  -> del
    | false -> failwith "invalid RTT / causality error. This is a bug"
    (* we are confident leaving this as an exception and returning a Maybe because the only way it can be reached is if the
     * TSC counter counts backwards or the code somehow invoked it in a bad way. There is no way that data from the network
     * can cause this exception to be reached, only a devastating bug in the code or the TSC counter being used violating its
     * invariants
     *)

let rtt_of sample =
    rtt_of_prime (fst sample)

let check_positive x =
    match (x > 0.0) with
    | true -> x
    | false -> failwith "should be positive!"

let check_non_negative x =
    match (x >= 0.0) with
    | true -> x
    | false -> failwith "should never be negative!"

let error_of packet rtt_hat =
    delta_TSC (rtt_of packet) rtt_hat

let dTSC p_hat del =
    p_hat *. (Int64.to_float del)

(*    th_naive = (
 *    peer->phat * ((long double)stamp->Ta + (long double)stamp->Tf)
 *    + (2 * peer->C - (stamp->Tb + stamp->Te))
 *    ) / 2.0;
 *
 *)
let theta_of p_hat c sample =
    let ts = (fst sample).timestamps in
    let sumLocal    = Int64.to_float    ts.ta   +.  Int64.to_float  ts.tf in
    let sumFar      =                   ts.tb   +.                  ts.te in

    let twice_theta = p_hat *. sumLocal +. (2.0 *. c -. sumFar) in

    twice_theta /. 2.0

let plocal_theta_of         p_hat c p_local latest  sample =
    let theta   =  theta_of p_hat c                 sample in

    match p_local with
    | None          -> theta
    | Some p_local  -> theta -. (p_local -. p_hat) *. Int64.to_float (baseline latest sample)

let rate_of_pair newer_sample older_sample =
    let newer = (fst newer_sample).timestamps in
    let older = (fst older_sample).timestamps in
    let delTa = delta_TSC newer.ta  older.ta in
    let delTb = newer.tb -.         older.tb in
    let delTe = newer.te -.         older.te in
    let delTf = delta_TSC newer.tf  older.tf in

    match (delTa > 0L, delTb > 0.0, delTe > 0.0, delTf > 0L) with
    | (true,true, true, true) ->
            let forwards    = delTb /. (Int64.to_float delTa) in
            let reverse     = delTe /. (Int64.to_float delTf) in
            Some ((forwards +. reverse) /. 2.0)
    | (_,   _,    _,    _   ) -> None


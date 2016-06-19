open History
open Wire


type counter = Cstruct.uint64 [@printer fun fmt -> fprintf fmt "0x%Lx"]
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


type regime = ZERO | WARMUP | NORMAL (* turns out we *do* need to distinguish between ZERO and WARMUP *)
[@@deriving show]

type estimators = {
    pstamp:                          point  option; (* this is a point within samples, not rtt_hat *)
    p_hat_and_error:        (float * float) option;
    p_local:                (float * float) option;
    c:                               float  option;
    theta_hat_and_error:    (float * float) option;
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


type sync_state = {
    regime:                 regime;
    parameters:             physical_parameters;
    samples_and_rtt_hat:   (sample * counter) history;  (* sample history and RTT_hat history are zipped together *)
    estimators:             estimators;
}
[@@deriving show]

(* packet zero gets stamps, rtt, and theta_naive.
 * packets > 0 get stamps, rtt
 * packets in warmup mode also get theta_naive
 * when switching from warmup mode, a synthetic history RTT_hat is created in end_warmup_thetahat
 * packets in normal mode also get theta_naive, RTT_hat.
 *
 * also process_RTT_full rewrites RTThat_hist on upward shift detected

type rtt_state = {
    rtt_hat:        float;
    rtt
 *)

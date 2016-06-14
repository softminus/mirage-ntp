open History
open Wire

let (>>=) x f =
    match x with
    | Some x    -> (f x)
    | None      -> None

let (<$>) f m =                 (* fmap *)
    match m with
     | Some x   -> Some (f x)
     | None     -> None

let (<*>) f m =
    match f with
    | Some ff   -> ff <$> m
    | None      -> None

let join x = x >>= (fun x -> x)

type counter = Cstruct.uint64 [@printer fun fmt -> fprintf fmt "0x%Lx"]
[@@deriving show]


type physical_parameters = {
    skm_scale:      float;
    ts_limit:       float;
    skm_rate:       float;
    e_offset:       float;
    e_offset_qual:  float;
    initial_p:     (float * float);
    shift_thres:    counter;
}
[@@deriving show]


type windows    = {
    toplevel:           (point * point);

    shift_detection:    (point * point);
    offset:             (point * point);

    plocal_far:         (point * point);
    plocal_near:        (point * point);

}
[@@deriving show]

let default_windows params poll_period =
    let history_scale = 3600 * 24 * 7 in        (* seconds *)
    let top_win_size    =               history_scale                           / poll_period in

    let shift_win       = max 100   (int_of_float @@ 1e8 *. params.ts_limit )   / poll_period in
    let offset_size     =           (int_of_float           params.skm_scale)   / poll_period in

    let toplevel            = (Newest, Older(Newest, top_win_size   - 1)) in
    let shift_detection     = (Newest, Older(Newest, shift_win      - 1)) in
    let offset              = (Newest, Older(Newest, offset_size    - 1)) in

    (* plocal stuff:
        * wwidth is the width of each of the plocal_far / plocal_near windows
        * plocal_span is *how far apart* those windows are from each other
    *)

    let plocal_span = 5 * offset_size   in
    let wwidth      = plocal_span / 5   in
    let plocal_near         = (Newest, Older(Newest, wwidth         - 1)) in

    (* this follows from lines 1240 and 1241 in sync_bidir.c *)
    let plocal_far_centre   =   Older(Newest, plocal_span + wwidth      ) in
    let plocal_far          =  (Newer(plocal_far_centre,    wwidth/2    ),
                                Older(plocal_far_centre,    wwidth/2 - 1)) in

    {toplevel; shift_detection; offset; plocal_far; plocal_near}

let default_parameters =
    let skm_scale       = 1024.0 in
    let ts_limit        = 1.5e-5 in
    let skm_rate        = 2e-7 in
    let e_offset        = 6.0 *. ts_limit in (* 6 = offset_ratio *)
    let e_offset_qual   = 3.0 *. e_offset in
    let initial_p       = (1e-9, 0.0) in
    let shift_thres     = 0L in
    {skm_scale; ts_limit; skm_rate; e_offset; e_offset_qual; initial_p; shift_thres}

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
    samples_and_rtt_hat:   (sample * counter option) history;
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

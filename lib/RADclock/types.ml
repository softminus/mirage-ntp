open History
open Wire


type physical_parameters = {
    ts_limit:       float;
    skm_rate:       float;
    e_offset:       float;
    e_offset_qual:  float;
}

let default_parameters =
    let ts_limit        = 1.5e-5 in
    let skm_rate        = 2e-7 in
    let e_offset        = 6.0 *. ts_limit in (* 6 = offset_ratio *)
    let e_offset_qual   = 3.0 *. e_offset in
    {ts_limit; skm_rate; e_offset; e_offset_qual}

type counter = Cstruct.uint64

type nonce = {
    tsc:    counter;    (* the TSC value when we send it *)
    txts:   ts;         (* the random number that was in the transmit_timestamp of the packet we sent *)
}

type timestamps = {
    ta:     counter;
    tb:     float;
    te:     float;
    tf:     counter;
}

type quality = NG | OK

type sample = {
    quality:    quality;
    ttl:        int;
    stratum:    Wire.stratum;
    leap:       Wire.leap;
    refid:      Cstruct.uint32;
    rootdelay:  float;
    rootdisp:   float;
    timestamps: timestamps;
}


type regime = ZERO | WARMUP | NORMAL (* turns out we *do* need to distinguish between ZERO and WARMUP *)



type estimators = {
    pstamp:                         point   option; (* this is a point within samples, not rtt_hat *)
    rtt_hat:                        counter history;
    p_hat_and_error:        (float * float) option;
    p_local:                        float   option;
    c:                              float   option;
    theta_hat_and_error:    (float * float) option;
}


type sync_state = {
    regime:         regime;
    samples:        sample  history;
    estimators:     estimators;
}


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

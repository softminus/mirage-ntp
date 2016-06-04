open History
open Wire
type counter = Cstruct.uint64

type nonce = {
    tsc:    counter;    (* the TSC value when we send it *)
    txts:   ts;         (* the random number that was in the transmit_timestamp of the packet we sent *)
}

let delta_TSC newer older =
    let del = Int64.sub newer older in
    match (del > 0L) with
    | true  -> del
    | false -> failwith "invalid ΔTSC!"


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


type regime = WARMUP | NORMAL



type estimators = {
    pstamp:                     point   option; (* this is a point within samples, not rtt_hat *)
    rtt_hat:                    counter history;
    p_hat_and_error:    (float * float) option;
    p_local:                    float   option;
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

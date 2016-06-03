open History
type counter = Cstruct.uint64

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


type regime = ZERO | WARMUP | NORMAL

type sync_state = {
    regime:         regime;
    stamps:         sample  history;
    rtt_hat:        counter history;
    theta_naive:    float   history;
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

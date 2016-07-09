(* simulates a network link with latency and loss *)

type n_params = {
    min_rtt:    float;

    scale:      float; (* utterly temporary *)

(*    quiet_rtt:          float;
    quiet_variance:     float;

    normal_rtt:         float;
    normal_variance:    float;

    quiet_ratio:        float;

    tail_rtt:           float; *)
    loss_rate:  float;
}

let rtt_of params rv =
    params.min_rtt +. rv *. params.scale

let lost rv packet =
    match (rv < Random.float(1))
    | true  -> Some packet
    | false -> None
    

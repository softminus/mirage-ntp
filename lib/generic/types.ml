open History

exception Counter_causality of int64

type counter = Cstruct.uint64 [@printer fun fmt -> fprintf fmt "0x%Lx"]
[@@deriving show]

type regime = ZERO | WARMUP | READY
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

    offset_sanity_zero:         float;
    offset_sanity_aging:        float;

    initial_p:         (float * float);
}
[@@deriving show]

type query_ctx = {
    when_sent:  counter;    (* the TSC value when we send it *)
    nonce:      ts;         (* the random number that was in the transmit_timestamp of the packet we sent *)
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
    quality:        quality;
    timestamps:     timestamps;
    private_data:   private_data;
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

type output = {
    skm_scale:                      float;
    freshness:                      counter;
    p_hat_and_error:        (float * float);
    p_local:                (float * float) option;
    ca_and_error:           (float * float);
}
[@@deriving show]

type windows    = {
    top_win_size:       int;
    toplevel:           (point * point);

    halftop_win:        (point * point);

    warmup_win:         (point * point);

    pstamp_win:         (point * point);

    shift_win:          (point * point);
    offset:             (point * point);

    plocal_far:         (point * point);
    plocal_near:        (point * point);
}
[@@deriving show]

type sync_state = {
    regime:                 regime;
    parameters:             physical_parameters;
    windows:                windows;
    samples_and_rtt_hat:   (sample * counter) history;  (* sample history and RTT_hat history are zipped together *)
    estimators:             estimators;
}
[@@deriving show]


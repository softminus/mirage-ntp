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

    e_shift:            float;
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

type timestamps = {
    ta:     counter;
    tb:     float               [@printer fun fmt -> fprintf fmt "%.09f"];
    te:     float               [@printer fun fmt -> fprintf fmt "%.09f"];
    tf:     counter;
}
[@@deriving show]

type quality = NG | OK
[@@deriving show]

type 'a sample = {
    quality:        quality;
    timestamps:     timestamps;
    private_data:   'a;
}
[@@deriving show]

type 'a estimators = {
    pstamp:                          point  option; (* this is a point within samples, not rtt_hat *)
    p_hat_and_error:        (float * float) option;
    p_local:                (float * float) option;
    c:                               float  option;
    theta_hat_and_error:    (float * float * ('a sample * counter)) option;
}
[@@deriving show]

type output = {
    skm_scale:          float;
    freshness:          counter;
    rate:               (float * float);
    local_rate:         (float * float) option;
    ca_and_error:       (float * float);
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

type 'a sync_state = {
    regime:                 regime;
    parameters:             physical_parameters;
    windows:                windows;
    samples_and_rtt_hat:   ('a sample * counter) history;  (* sample history and RTT_hat history are zipped together *)
    estimators:             'a estimators;
}
[@@deriving show]


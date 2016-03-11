(* clock interface stuff *)

open Ntp_wire

type clockstate = {
    stratum:            stratum;
    precision:          int;
    root_delay:         short_ts;
    root_dispersion:    short_ts;
    refid:              int32;
    reftime:            ts;
    leap:               leap;
}



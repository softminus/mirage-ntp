(* clock interface stuff *)

open Ntp_wire

type clockstate = {
    stratum:    stratum;
    precision:  int;
    rootdelay:  short_ts;
    rootdisp:   short_ts;
    refid:      int32;
    reftime:    ts;
    leap:       leap;
}



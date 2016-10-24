open Tsc_clock
open Wire

type ntp_private = {
    ttl:        int;
    stratum:    Wire.stratum;
    leap:       Wire.leap_flavor;
    refid:      Cstruct.uint32  [@printer fun fmt -> fprintf fmt "0x%lx"];
    rootdelay:  float;
    rootdisp:   float;
}
type query_ctx = {
    when_sent:  counter;    (* the TSC value when we send it *)
    nonce:      ts;         (* the random number that was in the transmit_timestamp of the packet we sent *)
}
[@@deriving show]

type ntp_context = {
    inflight_query: query_ctx option;
    tsc_state:      ntp_private sync_state;
}

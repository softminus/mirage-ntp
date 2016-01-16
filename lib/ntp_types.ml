open Ntp_wire

type port = Cstruct.uint16

type peerstate = {
    (* config *)
    srcaddr:    Ipaddr.t;
    srcport:    port;
    dstaddr:    Ipaddr.t;
    dstport:    port;

    (* packet variables *)
    leap:       leap;
    version:    version;
    mode:       mode;
    stratum:    stratum;
    ppoll:      int;
    rootdelay:  short_ts;
    rootdisp:   short_ts;
    refid:      Cstruct.uint32;
    reftime:    ts;
    (* timestamps *)
    orgn:       ts;     (* our time,   when our packet left us *)
    recv:       ts;     (* their time, when our packet hit them *)
    xmt:        ts;     (* their time, when their reply left them *)
    return:     ts;     (* our time,   when their reply hit us *)

    (* statistics variables *)


}







(*

offset   theta     clock offset
delay    delta     round trip delay
disp     epsilon   dispersion
jitter   psi       jitter
filter   filter    clock filter
tp       t p       filter time
*)

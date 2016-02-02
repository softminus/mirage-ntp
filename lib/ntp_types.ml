open Ntp_wire

(* the NTP RFCs use both greek letters ("theta") and words ("offset") to refer
 * to the same things and it is frustrating. here is the correspondence between
 * the two:
     * clock offset     = theta
     * round-trip delay = delta
     * dispersion       = epsilon
     * peer jitter      = psi
 *  
 *  variables with _i    suffix are samples
 *  variables with _e suffix are estimates/averages
 *)

type lastrecv = {
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

    (* computed stuff from now on *)
    return_i:   ts;     (* our time,   when their reply hit us *)
}

type f_point = {
    t_i:        ts;
    offset_i:   float; (* clock offset *)
    delay_i:    float; (* round-trip delay *)
    disp_i:     float; (* dispersion *)
}

type stats = {
    (* statistics variables *)
    offset_e:   float; (* offset from us *)
    delay_e:    float; (* delay  away from us *)
    disp_e:     float; (* peer dispersion *)
    jitter_e:   float; (* RMS jitter *)

    filter:     f_point array;
}

type pollvars = {
    hpoll:      int;
    burst:      int;
    reach:      int;
    ttl:        int;
    unreach:    int;
    outdate:    int;
    nextdate:   int;
}

type port = Cstruct.uint16

type peerstate = {
    (* config *)
    srcaddr:    Ipaddr.t;
    srcport:    port;
    dstaddr:    Ipaddr.t;
    dstport:    port;

    last:       lastrecv;
    stats:      stats;
    poll:       pollvars;
}

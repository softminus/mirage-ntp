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
 *  variables with _hat  suffix are estimates/averages
 *)


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

    (* computed stuff from now on *)
    return:     ts;     (* our time,   when their reply hit us *)

    (* statistics variables *)
    offset:     double; (* offset from us *)
    delay:      double; (* delay  away from us *)
    disp:       double; (* peer dispersion *)   
    jitter:     double; (* RMS jitter *)


}
type f_point = {
    t_i:        ts;
    offset_i:   double; (* clock offset *)
    delay_i:    double; (* round-trip delay *)
    disp_i:     double; (* dispersion *)
}








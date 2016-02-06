open Ntp_wire

(* the NTP RFCs use both greek letters ("theta") and words ("offset") to refer
 * to the same things and it is frustrating. here is the correspondence between
 * the two:
     * clock offset     = theta
     * round-trip delay = delta
     * dispersion       = epsilon
     * peer jitter      = psi
 *  
 *  variables with _i suffix are samples
 *  variables with _e suffix are estimates/averages
 *
 *  Also, nomenclature like "origin timestamp", "receive timestamp", "transmit timestamp",
 *  and "destination timestamp" is confusing and ambiguous. Instead, here:
     *  variables beginning with ne_ (near end) are times measured/struck on/by our host
     *  variables beginning with fe_ (far  end) are times measured/struck on/by the server we query
 *
 *  Therefore, when we create a packet, we strike   ne_transmit
 *  when it arrives at the server,      it strikes  fe_receive
 *  when it sends back a reply,         it strikes  fe_transmit
 *  when we receive the reply,          we strike   ne_receive
 *
 *
 *
 *)

type recv = {           (* we update this when we RECEIVE a packet from the server *)
    (* packet variables *)
    leap:           leap;
    version:        version;
    mode:           mode;
    stratum:        stratum;
    ppoll:          int;
    rootdelay:      short_ts;
    rootdisp:       short_ts;



    refid:          Cstruct.uint32;
    reftime:        ts;
    (* timestamps in packet*)
    ne_transmit:    ts;     (* known in NTP docs as "origin timestamp"      or "T_1" *)
    fe_receive:     ts;     (*                  aka "receive timestamp"     or "T_2" *) 
    fe_transmit:    ts;     (*                  aka "transmit timestamp"    or "T_3" *) 

    (* timestamp that we measure when the reply gets back to us *)
    ne_receive:     ts;     (*                  aka "destination timestamp" or "T_4" *)
}

type sent = {           (* we update this when we SEND a packet to the server *)
    ne_transmit:    ts;
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

type poll = {
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

    poll:       poll;

    sent:       sent;
    recv:       recv;
    stats:      stats;
}

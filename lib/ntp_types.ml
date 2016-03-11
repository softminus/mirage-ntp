open Ntp_wire


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

type send = {           (* we update this when we SEND a packet to the server *)
    ne_transmit:    ts;
}


type port = Cstruct.uint16

type peerstate = {
    (* config *)
    srcaddr:    Ipaddr.t;
    srcport:    port;
    dstaddr:    Ipaddr.t;
    dstport:    port;


    send:       send;
    recv:       recv;
}

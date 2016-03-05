open Ntp_wire

(* the NTP RFCs use both greek letters ("theta") and words ("offset") to refer
 * to the same things and it is frustrating. here is the correspondence between
 * the two:
     * clock offset     = theta
     * round-trip delay = delta
     * dispersion       = epsilon
     * peer jitter      = psi
 *
 * variables with _i suffix are samples
 * variables with _e suffix are estimates/averages
 * variables beginning with ne_ (near end) are times measured/struck on/by our host
 * variables beginning with fe_ (far  end) are times measured/struck on/by the server we query
 *
 * Therefore, when we create a packet, we strike   ne_transmit
 * when it arrives at the server,      it strikes  fe_receive
 * when it sends back a reply,         it strikes  fe_transmit
 * when we receive the reply,          we strike   ne_receive
 *
 *
 *
 * The NTP protocol is symmetric and simple and yet the official documentation
 * tries its best to obscure the simplicity. Every packet sent by a full
 * implementation contains:

     * a timestamp struck by the sender when it is created 
        (known as "transmit timestamp")
     * a timestamp struck by the sender when the last packet from its interlocutor was received
        (known as "receive timestamp")
     * a copy of what was in the "transmit timestamp" field in the last packet received from its interlocutor
        (known as "originator timestamp")

 * Due to the symmetry of the NTP wire protocol, ne_transmit is in the
 * "transmit timestamp" field when we sent a packet but is returned to us in
 * the "origin timestamp" field.
 *
 *
 * We implement a useful and clean subset (that is not SNTP!) of the NTP
 * protocol that only permits use of the server and client modes. Nothing in
 * the NTP wire protocol is asymmetric -- it permits two hosts that are
 * exchanging NTP packets to each measure the offset and delay between each
 * other. However, this symmetry is not needed in either the client or server
 * modes and an implementation without it avoids needless complexity and
 * obtains exactly the same results.
 *
 * Rather, the packets we send when in client mode only have a *single*
 * timestamp field filled out -- the "transmit timestamp" field, with a
 * timestamp we strike when we create that packet. We could actually fill it
 * with a completely random number/nonce and store a nonce->timestamp mapping
 * table locally, as the server does not process it beyond copying it into the
 * "originator timestamp" field in its reply.
 *
 * We don't fill out the other two timestamp fields in the NTP packet that
 * would let the server measure our offset/delay, nor do we fill out the
 * "reference timestamp" field.
 *
 * The server will reply with a packet with the same information in the
 * timestamps as it would for a packet with the "originator" and "receive"
 * timestamps filled -- indeed, the first packet every NTP client sends to a
 * server can't have those fields filled.
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

type send = {           (* we update this when we SEND a packet to the server *)
    ne_transmit:    ts;
}


type sample = {
    ne_recv:            Ntp_wire.span;      (* the time we received the packet that gave us this sample *)
    offset:             Ntp_wire.seconds;   (* clock offset *)
    delay:              Ntp_wire.seconds;   (* round-trip delay *)
    dispersion:         Ntp_wire.seconds;   (* dispersion in the original measurement *)
    total_dispersion:   Ntp_wire.seconds;   (* total dispersion = p_dispersion_i + phi*(t - t_0)        *)
}

type stats = {
    (* statistics variables *)
    offset_e:       Ntp_wire.seconds; (* offset from us *)
    delay_e:        Ntp_wire.seconds; (* delay  away from us *)
    dispersion_e:   Ntp_wire.seconds; (* peer dispersion *)
    jitter_e:       Ntp_wire.seconds; (* RMS jitter *)

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

    send:       send;
    recv:       recv;
    stats:      stats;
    filter:     sample list;
}

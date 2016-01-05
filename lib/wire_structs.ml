type ts = {
    seconds: Cstruct.uint32; 
    fraction: Cstruct.uint32;
}

let ts_to_int64 ts =
    Int64.add (Int64.of_int32 ts.fraction) (Int64.shift_left (Int64.of_int32 ts.seconds) 32)


type short_ts = {
    seconds: Cstruct.uint16; 
    fraction: Cstruct.uint16;
}

let short_ts_to_int32 ts =
    Int32.add (Int32.of_int ts.fraction) (Int32.shift_left (Int32.of_int ts.seconds) 16)

type date = {
    era: Cstruct.uint32;
    offset: Cstruct.uint32;
    fraction: Cstruct.uint64;
}
cstruct ntp {
    uint8_t     flags;
    uint8_t     stratum;
    int8_t      poll;
    int8_t      precision;
    uint32_t    root_delay;
    uint32_t    root_dispersion;
    uint32_t    refid;

    uint64_t    reference_ts;    (* not really an important timestamp *)


    (* the important timestamps *)

    uint64_t    origin_ts;   (* T1: client-measured time when request departs *)
    uint64_t    recv_ts;     (* T2: server-measured time when request arrives *)
    uint64_t    trans_ts;    (* T3: server-measured time when reply   departs *)
} as big_endian


type pkt = {
    flags           : int;
    stratum         : int;
    poll            : int;
    precision       : int;
    root_delay      : short_ts;
    root_dispersion : short_ts;
    refid           : int32;
    reference_ts    : ts;
    origin_ts       : ts;
    recv_ts         : ts;
    trans_ts        : ts;
}

let buf_of_pkt p =
    let buf = Cstruct.create sizeof_ntp in
    set_ntp_flags           buf                     p.flags;
    set_ntp_stratum         buf                     p.stratum;
    set_ntp_poll            buf                     p.poll;
    set_ntp_precision       buf                     p.precision;
    set_ntp_root_delay      buf (short_ts_to_int32  p.root_delay);
    set_ntp_root_dispersion buf (short_ts_to_int32  p.root_dispersion);
    set_ntp_refid           buf                     p.refid;
    set_ntp_reference_ts    buf (ts_to_int64        p.reference_ts);
    set_ntp_origin_ts       buf (ts_to_int64        p.origin_ts);
    set_ntp_recv_ts         buf (ts_to_int64        p.recv_ts);
    set_ntp_trans_ts        buf (ts_to_int64        p.trans_ts);



type ts = {
    seconds: Cstruct.uint32; 
    fraction: Cstruct.uint32;
}

type short_ts = {
    seconds: Cstruct.uint16; 
    fraction: Cstruct.uint16;
}

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
    uint16_t    root_delay[2];
    uint16_t    root_dispersion[2];
    uint32_t    refid;

    uint32_t    reference_ts[2];    (* not really an important timestamp *)


    (* the important timestamps *)

    uint32_t    origin_ts[2];   (* T1: client-measured time when request departs *)
    uint32_t    recv_ts[2];     (* T2: server-measured time when request arrives *)
    uint32_t    trans_ts[2];    (* T3: server-measured time when reply   departs *)
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


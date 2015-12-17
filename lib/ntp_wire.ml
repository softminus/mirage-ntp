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
module Ntp_wire = struct
    cstruct ntp {
        uint8_t     flags;
        uint8_t     stratum;
        int8_t      poll;
        int8_t      precisions;
        uint16_t    root_delay[2];
        uint16_t    root_dispersion[2];
        uint32_t    refid;

        (* the important timestamps *)
        
        uint32_t    ref_ts[2];
        uint32_t    origin_ts[2];
        uint32_t    recv_ts[2];
        uint32_t    tx_ts[2];



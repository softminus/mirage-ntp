type ts = {
    seconds: Cstruct.uint32; 
    fraction: Cstruct.uint32;
}

let ts_to_int64 ts =
    Int64.add (Int64.of_int32 ts.fraction) (Int64.shift_left (Int64.of_int32 ts.seconds) 32)

let int64_to_ts i =
    let seconds =   Int64.to_int32 (Int64.shift_right_logical i 32) in
    let fraction =  Int64.to_int32 i in
    {seconds; fraction}


type short_ts = {
    seconds: Cstruct.uint16; 
    fraction: Cstruct.uint16;
}

let short_ts_to_int32 ts =
    Int32.add (Int32.of_int ts.fraction) (Int32.shift_left (Int32.of_int ts.seconds) 16)

let int32_to_short_ts i =
    let seconds =  Int32.to_int(Int32.shift_right_logical i 16) in
    let fraction = Int32.to_int(Int32.logand (Int32.of_int 0xffff) i) in
    {seconds; fraction}

type date = {
    era: Cstruct.uint32;
    offset: Cstruct.uint32;
    fraction: Cstruct.uint64;
}

type leap = NoWarning | Minute61 | Minute59 | Unknown (* leap seconds were a mistake *)

type version = int

type mode = Reserved | SymA | SymP | Client | Server | Broadcast | Control | Private

let lvm_to_int l v m =
    let li = match l with
    | NoWarning -> 0 lsl 6
    | Minute61  -> 1 lsl 6
    | Minute59  -> 2 lsl 6
    | Unknown   -> 3 lsl 6 in
    let vi = v lsl 3 in
    let mi = match m with
    | Reserved  -> 0
    | SymA      -> 1
    | SymP      -> 2
    | Client    -> 3
    | Server    -> 4
    | Broadcast -> 5
    | Control   -> 6
    | Private   -> 7 in

    li + vi + mi


let flags_to_leap       f =
    match f lsr 6 with
    | 0 -> NoWarning
    | 1 -> Minute61
    | 2 -> Minute59
    | 3 -> Unknown
    | _ -> failwith ":("

let flags_to_version    f = (f land 0x38) lsr 3
let flags_to_mode       f =
    match (f land 0x07) with
    | 0 -> Reserved
    | 1 -> SymA
    | 2 -> SymP
    | 3 -> Client
    | 4 -> Server
    | 5 -> Broadcast
    | 6 -> Control
    | 7 -> Private
    | _ -> failwith ":("



type stratum = Invalid | Primary | Secondary of int | Unsynchronized | Reserved of int

let int_to_stratum (i: Cstruct.uint8) = match i with
    | 0 -> Invalid
    | 1 -> Primary
    | n1 when ((n1 > 1) && (n1 < 16)) -> Secondary n1
    | 16 -> Unsynchronized
    | n2  -> Reserved n2

let stratum_to_int s = match s with
    | Invalid -> 0
    | Primary -> 1
    | Secondary n -> n
    | Unsynchronized -> 16
    | Reserved n -> n



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
    leap            : leap;
    version         : version;
    mode            : mode;
    stratum         : stratum;
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
    set_ntp_flags           buf  (lvm_to_int p.leap p.version p.mode);
    set_ntp_stratum         buf     (stratum_to_int p.stratum);
    set_ntp_poll            buf                     p.poll;
    set_ntp_precision       buf                     p.precision;
    set_ntp_root_delay      buf (short_ts_to_int32  p.root_delay);
    set_ntp_root_dispersion buf (short_ts_to_int32  p.root_dispersion);
    set_ntp_refid           buf                     p.refid;
    set_ntp_reference_ts    buf (ts_to_int64        p.reference_ts);
    set_ntp_origin_ts       buf (ts_to_int64        p.origin_ts);
    set_ntp_recv_ts         buf (ts_to_int64        p.recv_ts);
    set_ntp_trans_ts        buf (ts_to_int64        p.trans_ts);

    buf




let pkt_of_buf b =
    if Cstruct.len b <> sizeof_ntp then
        None
    else
        let leap            = get_ntp_flags             b |> flags_to_leap      in
        let version         = get_ntp_flags             b |> flags_to_version   in
        let mode            = get_ntp_flags             b |> flags_to_mode      in
        let stratum         = get_ntp_stratum           b |> int_to_stratum     in
        let poll            = get_ntp_poll              b                       in
        let precision       = get_ntp_precision         b                       in
        let root_delay      = get_ntp_root_delay        b |> int32_to_short_ts  in
        let root_dispersion = get_ntp_root_dispersion   b |> int32_to_short_ts  in
        let refid           = get_ntp_refid             b                       in
        let reference_ts    = get_ntp_reference_ts      b |> int64_to_ts        in
        let origin_ts       = get_ntp_origin_ts         b |> int64_to_ts        in
        let recv_ts         = get_ntp_recv_ts           b |> int64_to_ts        in
        let trans_ts        = get_ntp_trans_ts          b |> int64_to_ts        in
        Some {leap;version;mode; stratum; poll; precision; root_delay; root_dispersion; refid; reference_ts; origin_ts; recv_ts; trans_ts}

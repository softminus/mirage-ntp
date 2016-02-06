open Ntp_wire
open Ntp_types

let validate_packet b ctx =
    let pkt = pkt_of_buf b in
    match pkt with
    | None -> None
    | Some p ->
            if p.version    <>  4                       then None else

            if p.trans_ts   =   int64_to_ts Int64.zero  then None else (* server not sync'd *)
            if p.recv_ts    =   int64_to_ts Int64.zero  then None else (* server not sync'd *)

            if p.trans_ts   <>  ctx.recv.fe_transmit    then None else (* we already saw this packet *)
            if p.origin_ts  <>  ctx.send.ne_transmit    then None else (* this packet doesn't have the timestamp
                                                                          we struck in it *)
            Some p

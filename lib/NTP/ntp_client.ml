(*
 * This corresponds to RADclock's client_ntp.c
 *
 * this part generates NTP queries and processes replies and creates samples
 *
 *)

open Wire

(* 
 * Generation of query packets (with us as client) to a server:
 *
 * We are mildly hostile here for simplicity purposes and we fill out the
 * reference timestamp with zeroes. Similarly, we fill the receive timestamp
 * and originator timestamp with zeroes. 
 *
 * The only timestamp that needs to not be zeroes is the transmit timestamp
 * because it'll get copied and returned to us in the server's reply (in the
 * "originator timestamp" field) and we need to remember it so we can bind each
 * reply packet from the server to the correct query packet we sent. 
 *
 * However, it need not be a timestamp, and indeed, we just choose a random
 * number to make off-path attacks a little more difficult.
 *
 *)


let allzero:ts = {seconds = Int32.of_int 0; fraction = Int32.of_int 0}

let new_query x = 
    let leap = Unknown in
    let version = 4 in
    let mode = Client in
    let stratum = Unsynchronized in
    let poll    = 4 in
    let precision = -6 in
    let root_delay = {seconds = 1; fraction = 0} in
    let root_dispersion = {seconds = 1; fraction = 0} in
    let refid = Int32.of_string "0x43414d4c" in
    let reference_ts = allzero in

    let origin_ts = allzero in
    let recv_ts = allzero in
    let trans_ts = x in 
    {leap;version;mode; stratum; poll; precision; root_delay; root_dispersion; refid; reference_ts; origin_ts; recv_ts; trans_ts}

let new_reply rxtime txtime qp curtime =
    let leap = NoWarning in
    let version = 4 in
    let mode = Server in
    let stratum = Primary in
    let poll = qp.poll in
    let precision = -6 in
    let root_delay = {seconds = 1; fraction = 0} in
    let root_dispersion = {seconds = 1; fraction = 0} in
    let refid = Int32.of_string "0x43414d4c" in

    let reference_ts = curtime in
    let origin_ts = qp.trans_ts in
    let recv_ts = rxtime in
    let trans_ts = txtime in
    {leap;version;mode; stratum; poll; precision; root_delay; root_dispersion; refid; reference_ts; origin_ts; recv_ts; trans_ts}

let validate_packet buf nonce=
    let pkt = pkt_of_buf buf in
    match pkt with
    | None -> None
    | Some p ->
            if p.version    <>  4                       then None else

            if p.trans_ts   =   int64_to_ts Int64.zero  then None else (* server not sync'd *)
            if p.recv_ts    =   int64_to_ts Int64.zero  then None else (* server not sync'd *)

            if p.origin_ts  <>  nonce                   then None else (* this packet doesn't have the timestamp
                                                                          we struck in it *)
            Some p

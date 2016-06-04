open History
open Wire
open Types
open Tsc

(*
 * This corresponds to RADclock's client_ntp.c
 *
 * this part generates NTP queries and processes replies and creates samples
 *
 *)

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

let query_pkt x =
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

let new_query =
    let tsc = Int64.of_int @@ rdtsc() in
    let txts:ts = int64_to_ts tsc in            (* FIXME: make this a random number *)
    let nonce = {tsc; txts} in
    (nonce, buf_of_pkt @@ query_pkt txts)

let validate_reply buf nonce =
    let pkt = pkt_of_buf buf in
    match pkt with
    | None -> None
    | Some p ->
            if p.version    <>  4                       then None else

            if p.trans_ts   =   int64_to_ts Int64.zero  then None else (* server not sync'd *)
            if p.recv_ts    =   int64_to_ts Int64.zero  then None else (* server not sync'd *)

            if p.origin_ts  <>  nonce.txts              then None else (* this packet doesn't have the timestamp
                                                                          we struck in it *)
            Some p

let sample_of_packet history nonce (pkt : pkt) rx_tsc =
    let l = get history Newest in
    let quality = match l with
    | None -> OK
    | Some last ->
            (* FIXME: check for TTL changes when we have a way to get ttl of
             * received packet from mirage UDP stack
             *)
            if pkt.leap     <> last.leap    then NG else
            if pkt.refid    <> last.refid   then NG else
            if pkt.stratum  <> last.stratum then NG else OK
    in
    let ttl         = 64 in
    let stratum     = pkt.stratum in
    let leap        = pkt.leap in
    let refid       = pkt.refid in
    let rootdelay   = short_ts_to_float pkt.root_delay in
    let rootdisp    = short_ts_to_float pkt.root_dispersion in
    let timestamps  = {ta = nonce.tsc; tb = to_float pkt.recv_ts; te = to_float pkt.trans_ts; tf = rx_tsc} in
    {quality; ttl; stratum; leap; refid; rootdelay; rootdisp; timestamps}

let new_sample_history old_state buf nonce rx_tsc =
    match (validate_reply buf nonce) with
    | None      ->  old_state
    | Some pkt  ->  {old_state with samples = hcons (sample_of_packet old_state.samples nonce pkt rx_tsc) old_state.samples}

let new_estimators state =
    match state.regime with

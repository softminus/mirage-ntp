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
open Wire
open Tsc_clock
open Maybe

type ntp_private = {
    ttl:        int;
    stratum:    Wire.stratum;
    leap:       Wire.leap;
    refid:      Cstruct.uint32  [@printer fun fmt -> fprintf fmt "0x%lx"];
    rootdelay:  float;
    rootdisp:   float;
}
type query_ctx = {
    when_sent:  counter;    (* the TSC value when we send it *)
    nonce:      ts;         (* the random number that was in the transmit_timestamp of the packet we sent *)
}
[@@deriving show]
let allzero:ts = {timestamp = 0x0L}

type ntp_context = {
    inflight_query: query_ctx option;
    tsc_state:      ntp_private sync_state;
}

let query_pkt nonce =
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
    let trans_ts = nonce in
    {leap;version;mode; stratum; poll; precision; root_delay; root_dispersion; refid; reference_ts; origin_ts; recv_ts; trans_ts}

let new_query when_sent rand =
    let nonce:ts = int64_to_ts rand in
    let queryctx = {when_sent; nonce} in
    (queryctx, buf_of_pkt @@ query_pkt nonce)

let validate_reply buf txctx =
    let pkt = pkt_of_buf buf in
    match pkt with
    | None -> None
    | Some p ->
            if p.version    <>  4                       then None else

            if p.trans_ts   =   int64_to_ts Int64.zero  then None else (* server not sync'd *)
            if p.recv_ts    =   int64_to_ts Int64.zero  then None else (* server not sync'd *)

            if p.origin_ts  <>  txctx.nonce             then None else (* this packet doesn't have the timestamp
                                                                          we struck in it *)
            Some p

let sample_of_packet latest_sample txctx (pkt : pkt) rx_tsc =
    let quality = match latest_sample with
    | None -> OK
    | Some (last, _) ->
            (* FIXME: check for TTL changes when we have a way to get ttl of
             * received packet from mirage UDP stack
             *)
            if pkt.leap     <> last.private_data.leap    then NG else
            if pkt.refid    <> last.private_data.refid   then NG else
            if pkt.stratum  <> last.private_data.stratum then NG else OK
    in
    let ttl         = 64 in
    let stratum     = pkt.stratum in
    let leap        = pkt.leap in
    let refid       = pkt.refid in
    let rootdelay   = short_ts_to_float pkt.root_delay in
    let rootdisp    = short_ts_to_float pkt.root_dispersion in
    (* print_string (Printf.sprintf "RECV TS IS %Lx" (ts_to_int64 pkt.recv_ts)); *)
    let timestamps  = {ta = txctx.when_sent; tb = to_float pkt.recv_ts; te = to_float pkt.trans_ts; tf = rx_tsc} in
    let private_data = {ttl; stratum; leap; refid; rootdelay; rootdisp} in

    let sample = {quality; timestamps; private_data} in
    sample

let initial_state =
    let tsc_state = blank_state in
    let inflight_query = None in
    {tsc_state; inflight_query}


let generate_query current_tsc state = (* returns a tuple of the query packet and the updated state *)
    let nonce = Cstruct.LE.get_uint64 (Nocrypto.Rng.generate 8) 0 in

    let (queryctx, packet) = new_query current_tsc nonce in

    let newstate = {state with inflight_query = Some queryctx} in

    (packet, newstate)



let process_reply current_tsc reply_packet state = (* returns the updated state *)
    let validated = join (validate_reply <$> reply_packet <*> state.inflight_query) in
    let sample = sample_of_packet (latest_sample state.tsc_state) <$> state.inflight_query <*> validated <*> Some current_tsc in
    let new_tsc_state = add_sample state.tsc_state <$> sample in

    match new_tsc_state with
    | Some upd  -> {state with tsc_state = upd}
    | None      ->  state


let output_of_state = 0

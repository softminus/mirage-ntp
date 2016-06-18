open History
open Wire
open Types
open Tsc
open Estimators

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

let blank_state =
    let regime              = ZERO                  in
    let samples_and_rtt_hat = History (100, 0, [])  in

    let pstamp              = None in
    let p_hat_and_error     = None in
    let p_local             = None in
    let c                   = None in
    let theta_hat_and_error = None in
    let estimators          = {pstamp; p_hat_and_error; p_local; c; theta_hat_and_error} in
    let parameters          = default_parameters in
    {regime; parameters; samples_and_rtt_hat; estimators}


let allzero:ts = {timestamp = 0x0L}

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

let new_query tsc =
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
    | Some (last, _) ->
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
    (* print_string (Printf.sprintf "RECV TS IS %Lx" (ts_to_int64 pkt.recv_ts)); *)
    let timestamps  = {ta = nonce.tsc; tb = to_float pkt.recv_ts; te = to_float pkt.trans_ts; tf = rx_tsc} in
    let sample = {quality; ttl; stratum; leap; refid; rootdelay; rootdisp; timestamps} in

    let rtt = rtt_of_prime sample in
    let rtt_hat = match l with
    | None                  -> rtt
    | Some (_, last_rtt)    ->  match (rtt < last_rtt) with
                                | true  -> rtt
                                | false -> last_rtt
    in
    (sample, rtt_hat)

let add_sample old_state buf nonce rx_tsc =
    match (validate_reply buf nonce) with
    | None      ->  old_state
    | Some pkt  -> {old_state with samples_and_rtt_hat = hcons (sample_of_packet old_state.samples_and_rtt_hat nonce pkt rx_tsc) old_state.samples_and_rtt_hat}

let output_of_state state =
    let e = state.estimators in
    match get state.samples_and_rtt_hat Newest with
    | None                      -> None
    | Some (sample, rtt_hat)    ->
            match (e.p_hat_and_error, e.c, e.theta_hat_and_error) with
            | (Some (p_hat, p_error), Some c, Some (th, th_err)) ->
                    let ca_and_error    = (c -. th, th_err) in
                    let p_hat_and_error = (p_hat, p_error) in
                    let freshness       = sample.timestamps.tf in
                    let p_local         = state.estimators.p_local in
                    let skm_scale       = state.parameters.skm_scale in
                    Some {skm_scale; freshness; p_hat_and_error; p_local; ca_and_error}
            | _ ->  None


let update_estimators old_state =
    match old_state.regime with
    | ZERO      ->
            let samples             = old_state.samples_and_rtt_hat in
            let p_hat_and_error     = Some old_state.parameters.initial_p in
            let p_local             = None in
            let theta_hat_and_error = None in

            let pstamp          = join  (warmup_pstamp      <$> (subset_warmup_pstamp       samples)) in
            (* Second stage estimators: *)
            let c               = join  (warmup_C_oneshot   <$> (fst <$> p_hat_and_error)
                                                            <*> (subset_warmup_C_oneshot    samples)) in

            let new_ests = {pstamp;  p_hat_and_error; p_local; c; theta_hat_and_error} in
            {old_state with estimators = new_ests; regime = WARMUP }

    | WARMUP    ->
            let samples     = old_state.samples_and_rtt_hat in
            let rtt_hat     = snd <$> rtt_hat_point samples in

            let old_ests    = old_state.estimators in
            let p_local     = None in

            let pstamp          = join  (warmup_pstamp      <$> (subset_warmup_pstamp       samples)) in

            (* Second stage estimators: *)

            let p_hat_and_error = join  (warmup_p_hat       <$> rtt_hat <*> (subset_warmup_p_hat    samples)) in
            let c               = join  (warmup_C_fixup     <$> old_ests.c <*> (fst <$> old_ests.p_hat_and_error) <*> (fst <$> p_hat_and_error) 
                                                            <*> (subset_warmup_C_fixup              samples)) in

            let theta_hat_and_error = join (warmup_theta_hat old_state.parameters <$> (fst <$> p_hat_and_error) <*> rtt_hat <*> c
                                                            <*> (subset_warmup_theta_hat            samples)) in

            let new_ests = {pstamp; p_hat_and_error; p_local; c; theta_hat_and_error} in

            {old_state with estimators = new_ests}

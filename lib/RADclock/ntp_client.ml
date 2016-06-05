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
    let regime      = ZERO                  in
    let samples     = History (100, 0, [])  in

    let pstamp              = None in
    let rtt_hat             = History (100, 0, []) in
    let p_hat_and_error     = None in
    let p_local             = None in
    let c                   = None in
    let theta_hat_and_error = None in
    let estimators          = {pstamp; rtt_hat; p_hat_and_error; p_local; c; theta_hat_and_error} in
    let parameters          = default_parameters in
    {regime; parameters; samples; estimators}


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
    print_string (Printf.sprintf "RECV TS IS %Lx" (ts_to_int64 pkt.recv_ts));
    let timestamps  = {ta = nonce.tsc; tb = to_float pkt.recv_ts; te = to_float pkt.trans_ts; tf = rx_tsc} in
    {quality; ttl; stratum; leap; refid; rootdelay; rootdisp; timestamps}

let add_sample old_state buf nonce rx_tsc =
    match (validate_reply buf nonce) with
    | None      ->   old_state
    | Some pkt  ->  {old_state with samples = hcons (sample_of_packet old_state.samples nonce pkt rx_tsc) old_state.samples}

let update_estimators old_state =
    match old_state.regime with
    | ZERO      ->
            let samples = old_state.samples in

            let pstamp  = Some    (run_estimator_1win warmup_pstamp                (win_warmup_pstamp  samples)) in
            let rtt_hat = hcons   (run_estimator_1win warmup_rtt_hat               (win_warmup_rtt_hat samples)) old_state.estimators.rtt_hat in

            let latest_rtt_hat  = point_of_range @@ range_of rtt_hat Newest Newest in
            let p_hat_and_error = Some (1e-9, 0.0) in

            let c                   =
                match (p_hat_and_error) with
                | Some (p, p_err)  -> run_estimator_1win (warmup_C_oneshot ~p_hat:p) (win_warmup_C_oneshot samples)
                | None              -> None
            in
            let p_local             =   None in
            let theta_hat_and_error =   None in
            let new_ests = {pstamp; rtt_hat; p_hat_and_error; p_local; c; theta_hat_and_error} in
            {old_state with estimators = new_ests; regime = WARMUP }


    | WARMUP    ->
            let samples     = old_state.samples in
            let old_ests    = old_state.estimators in

            let pstamp  = Some    (run_estimator_1win warmup_pstamp                (win_warmup_pstamp  samples)) in
            let rtt_hat = hcons   (run_estimator_1win warmup_rtt_hat               (win_warmup_rtt_hat samples)) old_state.estimators.rtt_hat in

            (* Second stage estimators: *)

            let latest_rtt_hat  = point_of_range @@ range_of rtt_hat Newest Newest in
            let p_hat_and_error = run_estimator_2win (warmup_p_hat ~rtt_hat:latest_rtt_hat) (win_warmup_p_hat   samples) in

            let c                   =
                match (old_ests.c, old_ests.p_hat_and_error, p_hat_and_error) with
                | (Some old_c, Some (old_p, old_err), Some (new_p, new_err))  ->
                        run_estimator_1win (warmup_C_fixup ~old_C:old_c ~old_p_hat:old_p ~new_p_hat:new_p) (win_warmup_C_fixup samples)
                | _                 -> None
            in
            let p_local             =   None in

            let theta_hat_and_error =
                match (p_hat_and_error, c) with
                | (Some (new_p, new_err), Some kc)  ->
                        run_estimator_2win (warmup_theta_hat ~params:old_state.parameters ~p_hat:new_p ~rtt_hat:latest_rtt_hat ~c:kc) (win_warmup_theta_hat samples)
                | _                 -> None
            in

            let new_ests = {pstamp; rtt_hat; p_hat_and_error; p_local; c; theta_hat_and_error} in
            {old_state with estimators = new_ests}


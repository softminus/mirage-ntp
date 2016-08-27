open History
open Types
open Estimators
open Maybe

(*
 * This just adds data to the state and cranks the estimators and extracts the
 * current values of the estimators when desired.
 *)

let blank_state =
    let regime              = ZERO in

    let pstamp              = None in
    let p_hat_and_error     = None in
    let p_local             = None in
    let c                   = None in
    let theta_hat_and_error = None in
    let estimators          = {pstamp; p_hat_and_error; p_local; c; theta_hat_and_error} in
    let parameters          = default_parameters in
    let windows             = default_windows parameters 16 in
    let samples_and_rtt_hat = History (windows.top_win_size, 0, [])  in
    {regime; parameters; samples_and_rtt_hat; estimators; windows}

let output_of_state state =
    let e = state.estimators in
    match get state.samples_and_rtt_hat Newest with
    | None                      -> None
    | Some (sample, rtt_hat)    ->
            match (e.p_hat_and_error, e.c, e.theta_hat_and_error) with
            | (Some (p_hat, p_error), Some c, Some (th, th_err, t_point)) ->
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

            let pstamp  = join  (warmup_pstamp <$>
                                (subset_warmup_pstamp       samples)) in

            let c       = join  (warmup_C_oneshot <$>
                                p_hat_and_error   <*>
                                (subset_warmup_C_oneshot    samples)) in

            let new_ests = {pstamp;  p_hat_and_error; p_local; c; theta_hat_and_error} in
            {old_state with estimators = new_ests; regime = WARMUP}

    | WARMUP    ->
            let samples     = old_state.samples_and_rtt_hat in
            let wi          = old_state.windows     in
            let old_ests    = old_state.estimators  in
            let params      = old_state.parameters  in


            let pstamp          = join  (warmup_pstamp  <$> (subset_warmup_pstamp       samples)) in

            (* Second stage estimators: *)

            let p_hat_and_error = join (warmup_p_hat <$> (subset_warmup_p_hat samples)) in


            let c = join    (c_fixup                    <$>
                            old_ests.c                  <*>
                            old_ests.p_hat_and_error    <*>
                            p_hat_and_error             <*>
                            (subset_c_fixup     samples)) in

            let p_local = None in

            let theta_hat_and_error = join (warmup_theta_hat params    <$>
                                        p_hat_and_error             <*>
                                        c                           <*>
                                        (subset_warmup_theta_hat            samples)) in


            let new_ests = {pstamp; p_hat_and_error; p_local; c; theta_hat_and_error} in

            let regime = match range_of_window wi.warmup_win samples with
            | None   -> WARMUP
            | Some x -> READY
            in

            {old_state with samples_and_rtt_hat = samples; estimators = new_ests; regime = regime}




    | READY     ->
            let samples     = old_state.samples_and_rtt_hat in
            let wi          = old_state.windows     in
            let old_ests    = old_state.estimators  in
            let params      = old_state.parameters  in


            let rtt_shift = join (detect_shift params <$> (shift_detection_subsets wi samples))  in
            let upshifted = upshift_rtts (upshift_edges    wi samples) <$> rtt_shift <*> (Some samples) in
            let samples = match upshifted with
            | Some x    -> x
            | None      -> samples
            in

            let pstamp          = join  (normal_pstamp  <$> (subset_normal_pstamp   wi  samples)) in

            (* Second stage estimators: *)

            let p_hat_normal = join (normal_p_hat params                    <$>
                                    (join (get samples <$> pstamp))         <*>
                                    old_ests.p_hat_and_error                <*>
                                    (latest_normal_p_hat                            wi  samples)) in

            let p_hat_and_error = p_hat_normal in

            let c = join    (c_fixup                    <$>
                            old_ests.c                  <*>
                            old_ests.p_hat_and_error    <*>
                            p_hat_and_error             <*>
                            (subset_c_fixup     samples)) in

            let p_local_normal  = join  (normal_p_local params  <$>
                                        p_hat_and_error         <*>
                                        (old_ests.p_local <|> p_hat_and_error) <*>
                                        (subset_normal_p_local          wi  samples)) in
            let p_local = p_local_normal <|> old_ests.p_local in

            let theta_hat_normal = join (normal_theta_hat params    <$>
                                        p_hat_and_error             <*>
                                        (Some p_local)              <*>
                                        c                           <*>
                                        old_ests.theta_hat_and_error<*>
                                        (subset_normal_theta_hat        wi  samples)) in
            let theta_hat_and_error = theta_hat_normal in

            let new_ests = {pstamp; p_hat_and_error; p_local; c; theta_hat_and_error} in

            {old_state with samples_and_rtt_hat = samples; estimators = new_ests}


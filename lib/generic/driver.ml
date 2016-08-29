open History
open Types
open Estimators
open Maybe

(*
 * This just adds data to the state and cranks the estimators and extracts the
 * current values of the estimators when desired.
 *)

let default_parameters =
    (* physical constants *)
    let skm_scale           = 1024.0 in
    let ts_limit            = 1.5e-5 in
    let skm_rate            = 2e-7 in
    let rate_error_bound    = 5.0e-7 in

    (* initial guess of timecounter frequency *)
    let initial_p       = (1e-9, 0.0) in

    (* shift detection, only shift_thres is used in the end *)
    let e_shift         = 10.0 *. ts_limit in
    let shift_thres     = 1L in

    (* p_hat *)

    (* point error threshold for using the current point to update p_hat *)
    let point_error_thresh = 3.0 *. ts_limit in
    (* error threshold for p_hat estimate *)
    let rate_error_threshold    = rate_error_bound /. 5.0 in
    (* bound on how fast p_hat can change over time *)
    let rate_sanity             = rate_error_bound *. 3.0 in

    (* p_local *)

    (* bound on how fast p_local can change over time *)
    let local_rate_sanity           = rate_error_bound *. 3.0   in
    (* error threshold for p_local estimate *)
    let local_rate_error_threshold  = 8.0e-7                    in

    (* theta *)

    (* used to calculate point error when doing theta *)
    let e_offset        = 6.0 *. ts_limit in (* 6 = offset_ratio *)
    (* to update a theta estimate, minimum point error must be less than e_offset_qual *)
    let e_offset_qual   = 3.0 *. e_offset in
    (* linear model for how offset estimator can be expected to change over a given baseline *)
    let offset_sanity_zero      = 100.0 *.  ts_limit            in
    let offset_sanity_aging     = 20.0 *.   rate_error_bound    in

    {skm_scale; ts_limit; skm_rate; e_offset; e_offset_qual; initial_p; e_shift; shift_thres; point_error_thresh; rate_error_threshold; rate_sanity; local_rate_sanity; local_rate_error_threshold; offset_sanity_zero; offset_sanity_aging}


let default_windows params poll_period =
    let history_scale = 3600 * 24 * 7 in        (* seconds *)
    let top_win_size    =               history_scale                           / poll_period  in

    let shift_win       = max 100  ((int_of_float @@ 1e8 *. params.ts_limit )   / poll_period) in
    let offset_size     =           (int_of_float           params.skm_scale)   / poll_period  in

    let toplevel            = (Newest, Older(Newest, top_win_size   - 1)) in
    let halftop_win         = (Newest, Older(Newest, top_win_size/2 - 1)) in
    let shift_win           = (Newest, Older(Newest, shift_win      - 1)) in
    let offset              = (Newest, Older(Newest, offset_size    - 1)) in

    (* plocal stuff:
        * wwidth is the width of each of the plocal_far / plocal_near windows
        * plocal_span is *how far apart* those windows are from each other
    *)

    let plocal_span = 5 * offset_size   in
    let wwidth      = plocal_span / 5   in
    let plocal_near         = (Newest, Older(Newest, wwidth         - 1)) in

    (* this follows from lines 1240 and 1241 in sync_bidir.c.
     *
     * yes, the way plocal_far_centre is defined ("+ wwidth") is kinda icky but
     * it lets us generate the same windows as RADclock when our plocal_span
     * equal to their plocal_win.
     *
     * Also how the far plocal window gets defined in sync_bidir.c:168 in
     * init_plocal (which is called once) seems contradictory with how the far
     * plocal window is defined afterwards, in process_plocal_full. This also
     * seems to violate the preconditions for calling radclock's
     * history_min_slide function in process_plocal_full.
     *
     * I chose to use the far plocal window definitions from
     * process_plocal_full because that is those definitions (and not the
     * init_plocal ones ) actually dictate which ranges RADclock (as of
     * version 73c1151afc24e42b131c47c1ee053e68ce8c075b) looks for plocal
     * points (see radclock_patches/window_bugs.patch for the code used to
     * verify that):
         * Note that radclock's history_min_slide(history *hist, index_t index_curr, index_t j, index_t i)
         * finds a minimum on [j+1,i+1], INCLUSIVE

     * TL;DR the C radclock implementation does weird things with indices but
     * the code here searches over the same intervals / same points as radclock
     * does. Furthermore, the radclock algorithms are not very sensitive to
     * changes in windowing / interval parameters as demonstrated in
     * doi:10.1145/1028788.1028817.
     *)

    let plocal_far_centre   =   Older(Newest, plocal_span + wwidth      ) in
    let plocal_far          =  (Newer(plocal_far_centre,    wwidth/2    ),
                                Older(plocal_far_centre,    wwidth/2 - 1)) in

    let phantom = History (top_win_size, 0, []) in
    let warmup_win = union_range phantom Newest (snd (union_range phantom Newest (snd shift_win) Newest (snd offset)))
                                         Newest (snd plocal_far                                                            ) in

    let warmup_len = match (range_length phantom (fst warmup_win) (snd warmup_win)) with
    | Some x -> x
    | None   -> failwith "invalid warmup interval"
    in
    let pstamp_win = (Newer(Oldest, warmup_len - 1), Oldest) in
    {top_win_size; toplevel; halftop_win; shift_win; offset; plocal_far; plocal_near; pstamp_win; warmup_win}

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

let rtt_dependent_parameters rtt_hat estimators parameters =
    match estimators.p_hat_and_error with
    | None              -> parameters
    | Some (p_hat, _)   ->
            match (p_hat * rtt_hat < 3e-3) with
            | true  -> {parameters with shift_thres = 

let output_of_state state =
    let e = state.estimators in
    match get state.samples_and_rtt_hat Newest with
    | None                      -> None
    | Some (sample, rtt_hat)    ->
            match (e.p_hat_and_error, e.c, e.theta_hat_and_error) with
            | (Some (p_hat, p_error), Some c, Some (th, th_err, t_point)) ->
                    let skm_scale       = state.parameters.skm_scale in
                    let freshness       = sample.timestamps.tf in
                    let rate            = (p_hat, p_error) in
                    let local_rate      = state.estimators.p_local in
                    let ca_and_error    = (c -. th, th_err) in
                    Some {skm_scale; freshness; rate; local_rate; ca_and_error}
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


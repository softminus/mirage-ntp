(* sync stuff *)

open Types
open History
open Maybe

type windows    = {
    toplevel:           (point * point);

    warmup_win:         (point * point);

    pstamp_win:         (point * point);

    shift_detection:    (point * point);
    offset:             (point * point);

    plocal_far:         (point * point);
    plocal_near:        (point * point);
}
[@@deriving show]

let default_windows params poll_period =
    let history_scale = 3600 * 24 * 7 in        (* seconds *)
    let top_win_size    =               history_scale                           / poll_period  in

    let shift_win       = max 100  ((int_of_float @@ 1e8 *. params.ts_limit )   / poll_period) in
    let offset_size     =           (int_of_float           params.skm_scale)   / poll_period  in

    let toplevel            = (Newest, Older(Newest, top_win_size   - 1)) in
    let shift_detection     = (Newest, Older(Newest, shift_win      - 1)) in
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
    let warmup_win = union_range phantom Newest (snd (union_range phantom Newest (snd shift_detection) Newest (snd offset)))
                                         Newest (snd plocal_far                                                            ) in

    let warmup_len = match (range_length phantom (fst warmup_win) (snd warmup_win)) with
    | Some x -> x
    | None   -> failwith "invalid warmup interval"
    in
    let pstamp_win = (Newer(Oldest, warmup_len - 1), Oldest) in
    {toplevel; shift_detection; offset; plocal_far; plocal_near; pstamp_win; warmup_win}



(* WARMUP ESTIMATORS
 *
 * No estimator (functions beginning with warmup_ and normal_) can have arguments
 * that are Maybe types. It is up to the caller to use the subset_ functions to
 * unwrap all the Maybes and give us unwrapped values.
 *)

let  subset_warmup_pstamp   ts          =       (* FOR: warmup_pstamp *)
    range_of ts Newest Oldest
let         warmup_pstamp   subset      =
    snd <$> (min_and_where rtt_of subset)       (* returns a Fixed *)




let  subset_warmup_p_hat    ts          =       (* FOR: warmup_p_hat *)
    let wwidth = 1 + (length ts) / 4 in
    let near    = range_of ts Newest @@ Older(Newest, wwidth - 1)                                       in
    let far     = range_of ts                                       (Newer(Oldest, wwidth - 1)) Oldest  in

    let latest = get ts Newest in
    ((fun x y z -> (x, y, z)) <$> near) <*> far <*> latest

let         warmup_p_hat subsets =
    let (near, far,     latest) = subsets in
    let rtt_hat = snd   latest in

    let best_in_near    = fst <$> (min_and_where rtt_of near) in
    let best_in_far     = fst <$> (min_and_where rtt_of far ) in
    let p_hat = join (rate_of_pair <$> best_in_near <*> best_in_far) in
    match (best_in_near, best_in_far, p_hat) with
    | (Some best_in_near, Some best_in_far, Some p) ->
            let del_tb      = check_non_negative ((fst best_in_near).timestamps.tb -. (fst best_in_far).timestamps.tb) in
            let far_error   = Int64.to_float @@ error_of best_in_far  rtt_hat in
            let near_error  = Int64.to_float @@ error_of best_in_near rtt_hat in
            let p_hat_error = (p /. del_tb) *. (far_error +. near_error) in
            Some (p, p_hat_error)
    | _ ->  None



(* C is only estimated once -- with first packet ever received! It is fixed up with
 * warmup_C_fixup to correct for change in p_hat but warmup_C_oneshot is never called
 * more than once. The theta estimators will compensate for the inevitable offset that
 * is inherent to C.
 *)
let  subset_warmup_C_oneshot    ts =        (* FOR: warmup_C_oneshot *)
    get ts Newest
let         warmup_C_oneshot p_hat subset =
    let first = fst subset in
    Some (first.timestamps.tb -. (dTSC p_hat first.timestamps.ta))




let  subset_warmup_C_fixup      ts =        (* FOR: warmup_C_fixup *)
    get ts Newest
let         warmup_C_fixup old_C old_p_hat new_p_hat subset =
    let newest = fst subset in
    Some (old_C +. (Int64.to_float newest.timestamps.ta) *. (old_p_hat -. new_p_hat))




let  subset_warmup_theta_hat    ts =        (* FOR: warmup_theta_hat *)
    let latest = get ts Newest in
    ((fun x y -> (x, y)) <$> latest) <*> range_of ts Newest Oldest

let warmup_theta_point_error params p_hat latest sa =
    let rtt_error   = dTSC p_hat @@ error_of sa (snd latest) in
    let age         = dTSC p_hat @@ baseline latest sa in
    rtt_error +. params.skm_rate *. age

let         warmup_theta_hat params p_hat c subsets =
    let (latest, subset) = subsets in

    let wt params p_hat latest sa =
        let qual = warmup_theta_point_error params p_hat latest sa in
        let weight = exp ( -. (qual *. qual) /. (params.e_offset *. params.e_offset)) in
        (* print_string (Printf.sprintf "weight calc, qual = %.9E, weight = %.9E\n" qual weight); *)
        weight
    in
    let sum, sum_wts =      weighted_sum (theta_of p_hat c) (wt params p_hat latest) subset in

    let min          =  min_and_where (warmup_theta_point_error params p_hat latest) subset in
    match min with
    | None -> None
    | Some min ->
            let minET               =  warmup_theta_point_error params p_hat latest @@ fst min in
            let theta_hat           =  sum /. check_positive(sum_wts) in
            match (minET < params.e_offset_qual) with
            | false -> None
            | true  -> Some (theta_hat, minET, latest)


(* NORMAL ESTIMATORS *)


let  subset_normal_rtt_entire   windows ts =    (* FOR: normal_RTT_hat halftop_subset *)
    range_of ts Newest Oldest
let  subset_normal_rtt_shift    windows ts =    (* FOR: normal_RTT_hat shift_subset *)
    let w = windows.shift_detection in
    range_of ts (fst w) (snd w)

let         normal_RTT_hat params halftop_subset shift_subset =     (* NOTE: halftop_subset is greater than shift_subset *)
    let subset_rtt     = rtt_of <$> (fst <$> min_and_where rtt_of halftop_subset) in
    let subsubset_rtt  = rtt_of <$> (fst <$> min_and_where rtt_of shift_subset  ) in

    (fun subsubset_rtt     subset_rtt -> match (subsubset_rtt > Int64.add subset_rtt params.shift_thres) with
                                            | false -> (subset_rtt,    false)   (* No upwards shift *)
                                            | true  -> (subsubset_rtt, true))   (* Upwards shift detected *)
    <$>  subsubset_rtt <*> subset_rtt




let  subset_upshift_samples     windows ts =    (* FOR: upshift_samples subset *)
    let x = windows.shift_detection in
    let y = windows.offset          in
    let inter = intersect_range ts (fst x) (snd x) (fst y) (snd y) in
    range_of ts (fst inter) (snd inter)
let         upshift_samples subsubset_rtt samples edges =
    let (left, right) = edges in
    slice_map samples left right (fun x -> (fst x, Some subsubset_rtt))




let  subset_normal_pstamp       windows ts =    (* FOR: normal_pstamp subset *)
    let w = windows.pstamp_win in
    range_of ts (fst w) (snd w)
let         normal_pstamp       subset =
    snd <$> (min_and_where rtt_of subset)       (* returns a Fixed *)




let  latest_normal_p_hat        windows ts =    (* FOR: normal_p_hat latest_and_rtt_hat *)
    get ts Newest
let         normal_p_hat params pstamp_and_rtt_hat old_p_hat latest_and_rtt_hat =
    let (pstamp, pstamp_rtt_hat)    = pstamp_and_rtt_hat in
    let (old_p,  old_p_err)         = old_p_hat in
    let (latest, latest_rtt_hat)    = latest_and_rtt_hat in
    let new_p = rate_of_pair latest pstamp in
    match new_p with
    | None      -> None     (* can't calculate a rate, return None *)
    | Some p    ->  match (dTSC old_p @@ error_of latest latest_rtt_hat < params.point_error_thresh) with
                    | false ->  None    (* point error of our new packet is NG, let's not use it *)

                    | true ->   let baseline      = ((fst latest).timestamps.tb -. (fst pstamp).timestamps.tb) in
                                let point_errors  = Int64.to_float @@ Int64.add (error_of latest latest_rtt_hat) (error_of pstamp pstamp_rtt_hat) in
                                let rtt_est_error = abs_float @@ Int64.to_float @@ Int64.sub latest_rtt_hat pstamp_rtt_hat in
                                let new_p_error = old_p *. (point_errors +. rtt_est_error) /. baseline in

                                match ((new_p_error < old_p_err), (new_p_error < params.rate_error_threshold)) with
                                | (false, false)    -> None (* it's worse than the last one and also not under the error threshold *)
                                | _                 ->  let change = abs_float @@ (p -. old_p) /. old_p in
                                                        match ((change < params.rate_sanity), (fst latest).quality) with
                                                        | (true, OK)    -> Some (p, new_p_error)
                                                        | _             -> Some (old_p, new_p_error)




let  subset_normal_C_fixup      windows ts =    (* FOR: normal_C_fixup *)
    get ts Newest
let         normal_C_fixup old_C old_p_hat new_p_hat subset =
    Some (old_C +. (Int64.to_float subset.timestamps.ta) *. (old_p_hat -. new_p_hat))




let  subset_normal_p_local      windows ts =    (* FOR: normal_p_local *)
    let near_win    = windows.plocal_near   in
    let near = range_of ts (fst near_win) (snd near_win)    in

    let far_win     = windows.plocal_far    in
    let far  = range_of ts (fst far_win)  (snd far_win)     in

    let latest = get ts Newest in

    ((fun x y z -> (x, y, z)) <$> near) <*> far <*> latest
let         normal_p_local params p_hat_and_error old_p_local subsets =
    let (p_hat,         _)      = p_hat_and_error in
    let (old_p_local,   _)      = old_p_local in
    let (near, far, latest)     = subsets in

    let rtt_hat                 = snd latest in

    let best_in_near    = fst <$> (min_and_where rtt_of near) in
    let best_in_far     = fst <$> (min_and_where rtt_of far ) in
    let rate            = join (rate_of_pair <$> best_in_near <*> best_in_far) in
    match (best_in_near, best_in_far, rate) with
    | (Some best_in_near, Some best_in_far, Some p_local) -> (
            let del_tb      = check_non_negative ((fst best_in_near).timestamps.tb -. (fst best_in_far).timestamps.tb) in
            let far_error   = Int64.to_float @@ error_of best_in_far  rtt_hat in
            let near_error  = Int64.to_float @@ error_of best_in_near rtt_hat in
            let plocal_error = (p_hat /. del_tb) *. (far_error +. near_error) in
            match (plocal_error < params.local_rate_error_threshold) with
            | false -> None
            | true  -> let change = abs_float @@ (p_local -. old_p_local) /. old_p_local in
                match ((change < params.local_rate_sanity), (fst latest).quality) with
                | (true, OK)    -> Some (p_local, plocal_error)
                | _             -> None)
    | _                         -> None




let max_gap win =
    let gap x y = baseline x y in
    let pairwise (acc, prev) z = match (acc, prev) with
        | (None,        None)       -> (None,                        Some z)
        | (None,        Some prev)  -> (Some          (gap prev z) , Some z)
        | (Some acc,    Some prev)  -> (Some (max acc (gap prev z)), Some z)

        | (Some acc,    None)       -> failwith "invalid state"
    in
    let ver = fold pairwise (None, None) win in
    match ver with
    | (None,        _     ) -> None
    | (Some best,   Some _) -> Some best
    | _                     -> failwith "invalid state!!"

let normal_theta_point_error params p_hat latest sa =
    let rtt_error   = dTSC p_hat @@ error_of sa (snd latest) in
    let age         = dTSC p_hat @@ baseline latest sa in
    rtt_error +. params.skm_rate *. age

let normal_theta_hat params p_hat p_local c old_theta_hat subsets =
    let (latest, offset_win) = subsets in
    let (old_theta, old_theta_error, old_theta_sample) = old_theta_hat in

    let wt params p_hat latest sa =
        let qual = normal_theta_point_error params p_hat latest sa in
        let weight = exp ( -. (qual *. qual) /. (params.e_offset *. params.e_offset)) in
        (* print_string (Printf.sprintf "weight calc, qual = %.9E, weight = %.9E\n" qual weight); *)
        weight
    in
    let sum, sum_wts =  weighted_sum (plocal_theta_of p_hat c p_local latest) (wt params p_hat latest) offset_win in

    let min          =  min_and_where (normal_theta_point_error params p_hat latest) offset_win in
    match min with
    | None      -> None
    | Some min  ->
            let minET       =          normal_theta_point_error params p_hat latest @@ fst min in
            let theta_hat   =   sum /. check_positive(sum_wts) in
            match (minET < params.e_offset_qual) with
            | false -> None
            | true  ->
                    let maxgap = max_gap offset_win in
                    match maxgap with
                    | None          -> None
                    | Some maxgap   ->
                            let maxgap = max (maxgap) (baseline latest old_theta_sample) in
                            let gap    = dTSC p_hat maxgap in
                            let change = abs_float @@ (old_theta -. theta_hat) in
                            match ((change < params.offset_sanity_zero +. gap *. params.offset_sanity_aging), (fst latest).quality) with
                            | (true, OK)    ->  Some (theta_hat, minET, latest)
                            | _             ->  None

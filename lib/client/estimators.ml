(* sync stuff *)

open Types
open History
open Maybe


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
let         warmup_C_oneshot p_hat_and_error subset =
    let first = fst subset in
    let (p_hat, _) = p_hat_and_error in
    Some (first.timestamps.tb -. (dTSC p_hat first.timestamps.ta))




let  subset_c_fixup      ts =        (* FOR: c_fixup *)
    get ts Newest
let         c_fixup old_C old_p_hat_and_error new_p_hat_and_error subset =
    let newest = fst subset in
    let (old_p_hat, _) = old_p_hat_and_error in
    let (new_p_hat, _) = new_p_hat_and_error in
    Some (old_C +. (Int64.to_float newest.timestamps.ta) *. (old_p_hat -. new_p_hat))





let warmup_theta_point_error params p_hat latest sa =
    let rtt_error   = dTSC p_hat @@ error_of sa (snd latest) in
    let age         = dTSC p_hat @@ baseline latest sa in
    rtt_error +. params.skm_rate *. age

let  subset_warmup_theta_hat    ts =        (* FOR: warmup_theta_hat *)
    let latest = get ts Newest in
    ((fun x y -> (x, y)) <$> latest) <*> range_of ts Newest Oldest
let         warmup_theta_hat params p_hat_and_error c subsets =
    let (latest, subset) = subsets in
    let (p_hat, _) = p_hat_and_error in

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



(* RTT upshift detection *)
let shift_detection_subsets                  windows                ts =    (* FOR: detect_shift *)
    let halftop_subset     = range_of_window windows.halftop_win    ts in
    let shift_subset       = range_of_window windows.shift_win      ts in
    ((fun x y -> (x, y)) <$> halftop_subset) <*> shift_subset

let detect_shift params subsets =
    let (halftop_subset, shift_subset) = subsets in         (* NOTE: halftop_subset is greater than shift_subset *)
    let halftop_rtt    = rtt_of <$> (fst <$> min_and_where rtt_of halftop_subset) in
    let detection_rtt  = rtt_of <$> (fst <$> min_and_where rtt_of shift_subset  ) in

    (fun  detection_rtt     halftop_rtt ->  match (detection_rtt > Int64.add halftop_rtt params.shift_thres) with
                                            | false -> None                 (* No upwards shift *)
                                            | true  -> Some detection_rtt   (* Upwards shift detected *)
    ) <$> detection_rtt <*> halftop_rtt



let upshift_edges windows ts =    (* FOR: upshift_rtts subset *)
    let x = windows.shift_win in
    let y = windows.offset          in
    intersect_range ts (fst x) (snd x) (fst y) (snd y)

let upshift_rtts edges rtt ts =
    let (left, right) = edges in
    slice_map ts left right (fun x -> (fst x, rtt))




(* NORMAL ESTIMATORS *)

let  subset_normal_pstamp       windows             ts =    (* FOR: normal_pstamp subset *)
    range_of_window             windows.pstamp_win  ts
let         normal_pstamp         subset =
    snd <$> (min_and_where rtt_of subset)       (* returns a Fixed *)




let  latest_normal_p_hat        windows ts =    (* FOR: normal_p_hat latest_and_rtt_hat *)
    get ts Newest
let         normal_p_hat params pstamp old_p_hat latest =
    let (old_p,  old_p_err)         = old_p_hat in
    let new_p = rate_of_pair latest pstamp in
    match new_p with
    | None      ->  Some old_p_hat
    | Some p    ->  match (dTSC old_p @@ error_of latest (snd latest) < params.point_error_thresh) with
                    | false ->  Some old_p_hat (* point error of our new packet is NG, let's not use it *)

                    | true ->   let baseline      = ((fst latest).timestamps.tb -. (fst pstamp).timestamps.tb) in
                                let point_errors  = Int64.to_float @@ Int64.add (error_of latest (snd latest)) (error_of pstamp (snd pstamp)) in
                                let rtt_est_error = abs_float @@ Int64.to_float @@ Int64.sub (snd latest) (snd pstamp) in
                                let new_p_error = old_p *. (point_errors +. rtt_est_error) /. baseline in

                                match ((new_p_error < old_p_err), (new_p_error < params.rate_error_threshold)) with
                                | (false, false)    ->  Some old_p_hat  (* it's worse than the last one and also not under the error threshold *)
                                | _                 ->  let change = abs_float @@ (p -. old_p) /. old_p in
                                                        match ((change < params.rate_sanity), (fst latest).quality) with
                                                        | (true, OK)    -> Some (p, new_p_error)
                                                        | _             -> Some (old_p, new_p_error)








let  subset_normal_p_local      windows             ts =    (* FOR: normal_p_local *)
    let near = range_of_window  windows.plocal_near ts in
    let far  = range_of_window  windows.plocal_far  ts in
    let latest = get                                ts Newest in

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





let normal_theta_point_error params p_hat latest sa =
    let rtt_error   = dTSC p_hat @@ error_of sa (snd latest) in
    let age         = dTSC p_hat @@ baseline latest sa in
    rtt_error +. params.skm_rate *. age

let  subset_normal_theta_hat            windows         ts =        (* FOR: normal_theta_hat *)
    let latest      = get ts Newest in
    let offset_win  = range_of_window   windows.offset  ts in
    ((fun x y -> (x, y)) <$> latest) <*> offset_win
let normal_theta_hat params p_hat_and_error p_local_and_error c old_theta_hat subsets =
    let (p_hat, _) = p_hat_and_error in
    let p_local    = fst <$> p_local_and_error in
    let (old_theta, old_theta_error, old_theta_sample) = old_theta_hat in

    let (latest, offset_win) = subsets in

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

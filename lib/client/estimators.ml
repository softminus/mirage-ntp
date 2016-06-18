(* sync stuff *)

open Types
open History

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


let (>>=) x f =                 (* bind *)
    match x with
    | Some x    -> (f x)
    | None      -> None

let (<$>) f m =                 (* fmap *)
    match m with
     | Some x   -> Some (f x)
     | None     -> None

let (<*>) f m =                 (* ap *)
    match f with
    | Some ff   -> ff <$> m
    | None      -> None

let (<|>) l r =
    match l with
    | None      -> r
    | x         -> x

let join x = x >>= (fun x -> x)
(* SHARED *)
let rtt_of sample =
    let ts = (fst sample).timestamps in
    let del = Int64.sub (ts.tf) (ts.ta) in
    match (del > 0L) with
    | true  -> del
    | false -> failwith "invalid RTT / causality error. This is a bug"
    (* we are confident leaving this as an exception and returning a Maybe because the only way it can be reached is if the
     * TSC counter counts backwards or the code somehow invoked it in a bad way. There is no way that data from the network
     * can cause this exception to be reached, only a devastating bug in the code or the TSC counter being used violating its
     * invariants
     *)

let check_positive x =
    match (x > 0.0) with
    | true -> x
    | false -> failwith "should be positive!"

let check_non_negative x =
    match (x >= 0.0) with
    | true -> x
    | false -> failwith "should never be negative!"

let error_of packet rtt_hat =
    delta_TSC (rtt_of packet) rtt_hat

let dTSC p_hat del =
    p_hat *. (Int64.to_float del)

(*    th_naive = (
 *    peer->phat * ((long double)stamp->Ta + (long double)stamp->Tf)
 *    + (2 * peer->C - (stamp->Tb + stamp->Te))
 *    ) / 2.0;
 *
 *)
let theta_of p_hat c sample =
    let ts = (fst sample).timestamps in
    let sumLocal    = Int64.to_float    ts.ta   +.  Int64.to_float  ts.tf in
    let sumFar      =                   ts.tb   +.                  ts.te in

    let twice_theta = p_hat *. sumLocal +. (2.0 *. c -. sumFar) in

    twice_theta /. 2.0


let rate_of_pair newer_sample older_sample =
    let newer = (fst newer_sample).timestamps in
    let older = (fst older_sample).timestamps in
    let delTa = delta_TSC newer.ta  older.ta in
    let delTb = newer.tb -.         older.tb in
    let delTe = newer.te -.         older.te in
    let delTf = delta_TSC newer.tf  older.tf in

    match (delTa > 0L, delTb > 0.0, delTe > 0.0, delTf > 0L) with
    | (true,true, true, true) ->
            let forwards    = delTb /. (Int64.to_float delTa) in
            let reverse     = delTe /. (Int64.to_float delTf) in
            Some ((forwards +. reverse) /. 2.0)
    | (_,   _,    _,    _   ) -> None



(* WARMUP ESTIMATORS
 *
 * No estimator (functions beginning with warmup_ and normal_) can have arguments
 * that are Maybe types. It is up to the caller to use the subset_ functions to
 * unwrap all the Maybes and give us unwrapped values.
 *)
let warmup_pstamp   subset =             snd <$> (min_and_where rtt_of subset)    (* returns a Fixed *)
let warmup_rtt_hat  subset = rtt_of <$> (fst <$> (min_and_where rtt_of subset))   (* returns the rtt number *)


let warmup_p_hat rtt_hat subsets =
    let (near, far) = subsets in
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
let warmup_C_oneshot p_hat r =
    let first = fst @@ point_of_history r in
    Some (first.timestamps.tb -. (dTSC p_hat first.timestamps.ta))

let warmup_C_fixup old_C old_p_hat new_p_hat latest =
    let newest = fst @@ point_of_history latest in
    Some (old_C +. (Int64.to_float newest.timestamps.ta) *. (old_p_hat -. new_p_hat))

let warmup_theta_point_error params p_hat rtt_hat latest sa =
    let rtt_error   = dTSC p_hat @@ error_of sa rtt_hat in
    let age         = dTSC p_hat (delta_TSC (fst latest).timestamps.tf (fst sa).timestamps.tf) in
    rtt_error +. params.skm_rate *. age

let warmup_theta_hat params p_hat rtt_hat c wins =
    let (last, subset) = wins in
    let latest = point_of_history last in

    let wt params p_hat rtt_hat latest sa =
        let qual = warmup_theta_point_error params p_hat rtt_hat latest sa in
        let weight = exp ( -. (qual *. qual) /. (params.e_offset *. params.e_offset)) in
        (* print_string (Printf.sprintf "weight calc, qual = %.9E, weight = %.9E\n" qual weight); *)
        weight
    in
    let sum, sum_wts =      weighted_sum (theta_of p_hat c) (wt params p_hat rtt_hat latest) subset in

    let min          =  min_and_where (warmup_theta_point_error params p_hat rtt_hat latest) subset in
    match min with
    | None -> None
    | Some min ->   (let minET        =                 warmup_theta_point_error params p_hat rtt_hat latest @@ fst min in
                    let theta_hat  =  sum /. check_positive(sum_wts) in
                    match (minET < params.e_offset_qual) with
                    | true  -> Some (theta_hat, minET)
                    | false -> None)


(* WARMUP SUBSETS *)

let subset_warmup_pstamp       ts =    (* FOR: warmup_pstamp *)
    range_of ts Newest Oldest

let subset_warmup_rtt_hat      ts =    (* FOR: warmup_rtt *)
    range_of ts Newest Oldest

let subset_warmup_p_hat        ts =    (* FOR: warmup_p_hat *)
    let wwidth = 1 + (length ts) / 4 in
    let near    = range_of ts Newest @@ Older(Newest, wwidth - 1)                                       in
    let far     = range_of ts                                       (Newer(Oldest, wwidth - 1)) Oldest  in

    ((fun x y -> (x, y)) <$> near) <*> far

let subset_warmup_C_oneshot    ts =    (* FOR: warmup_C_oneshot *)
    range_of ts Newest Oldest       (* Newest Oldest is used so if there's more than one sample, point_of_history
                                     * in warmup_C_oneshot will throw an exception!
                                     *)

let subset_warmup_C_fixup      ts =    (* FOR: warmup_C_fixup *)
    range_of ts Newest Newest

let subset_warmup_theta_hat    ts =    (* FOR: warmup_theta_hat *)
    let last = range_of ts Newest Newest in
    ((fun x y -> (x, y)) <$> last ) <*> range_of ts Newest Oldest

(* NORMAL ESTIMATORS *)

let normal_RTT_hat params   halftop_subset shift_subset = (* NOTE: halftop_subset is greater than shift_subset *)
    let subset_rtt     = rtt_of <$> (fst <$> min_and_where rtt_of halftop_subset) in
    let subsubset_rtt  = rtt_of <$> (fst <$> min_and_where rtt_of shift_subset  ) in

    (fun subsubset_rtt     subset_rtt -> match (subsubset_rtt > Int64.add subset_rtt params.shift_thres) with
                                            | false -> (subset_rtt,    false)   (* No upwards shift *)
                                            | true  -> (subsubset_rtt, true))   (* Upwards shift detected *)
    <$>  subsubset_rtt <*> subset_rtt

let handle_RTT_upshift subsubset_rtt samples subset =
    let (left, right) = subset in
    slice_map samples left right (fun x -> (fst x, Some subsubset_rtt))

let normal_pstamp   subset =             snd <$> (min_and_where rtt_of subset)    (* returns a Fixed *)

let normal_p_hat    params old_p_hat pstamp_and_rtt_hat latest_and_rtt_hat =
    let (pstamp, pstamp_rtt_hat)    = pstamp_and_rtt_hat in
    let (latest, latest_rtt_hat)    = latest_and_rtt_hat in
    let (old_p,  old_p_err)         = old_p_hat in
    let new_p = rate_of_pair latest pstamp in
    match new_p with
    | None      -> None     (* can't calculate a rate, return None *)
    | Some p    ->  match (dTSC old_p @@ error_of latest latest_rtt_hat < params.point_error_thresh) with
                    | false ->  None    (* point error of our new packet is NG, let's not use it *)
                    | true ->   let baseline      = ((fst latest).timestamps.tb -. (fst pstamp).timestamps.tb) in
                                let point_errors  = Int64.to_float @@ Int64.add (error_of latest latest_rtt_hat) (error_of pstamp pstamp_rtt_hat) in
                                let rtt_est_error = abs_float @@ Int64.to_float @@ Int64.sub latest_rtt_hat pstamp_rtt_hat in
                                let new_p_error = old_p *. (point_errors +. rtt_est_error) /. baseline in
                                Some new_p_error

(* NORMAL SUBSETS *)

let subset_normal_rtt_entire windows ts =   (* FOR: normal_RTT_hat halftop_subset *)
    range_of ts Newest Oldest

let subset_normal_rtt_shift windows ts =    (* FOR: normal_RTT_hat shift_subset *)
    let w = windows.shift_detection in
    range_of ts (fst w) (snd w)

let subset_normal_rtt_fixup windows ts =    (* FOR: handle_RTT_upshift subset *)
    let x = windows.shift_detection in
    let y = windows.offset          in
    let inter = intersect_range ts (fst x) (snd x) (fst y) (snd y) in
    range_of ts (fst inter) (snd inter)

let subset_normal_pstamp windows ts =       (* FOR: normal_pstamp subset *)
    let w = windows.pstamp_win in
    range_of ts (fst w) (snd w)

let subset_normal_p_hat windows ts =        (* FOR: normal_p_hat latest *)
    range_of ts Newest Newest


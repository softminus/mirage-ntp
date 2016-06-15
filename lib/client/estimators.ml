(* sync stuff *)

open Types
open History

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
    let delTa = Int64.sub newer.ta  older.ta in
    let delTb = newer.tb -.         older.tb in
    let delTe = newer.te -.         older.te in
    let delTf = Int64.sub newer.tf  older.tf in

    match (delTa > 0L, delTb > 0.0, delTe > 0.0, delTf > 0L) with
    | (true,true, true, true) ->
            let forwards    = delTb /. (Int64.to_float delTa) in
            let reverse     = delTe /. (Int64.to_float delTf) in
            Some ((forwards +. reverse) /. 2.0)
    | (_,   _,    _,    _   ) -> None



(* WARMUP ESTIMATORS *)
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
    Some (first.timestamps.tb -. (p_hat *. Int64.to_float first.timestamps.ta))

let warmup_C_fixup old_C old_p_hat new_p_hat latest =
    let newest = fst @@ point_of_history latest in
    Some (old_C +. (Int64.to_float newest.timestamps.ta) *. (old_p_hat -. new_p_hat))

let warmup_theta_point_error params p_hat rtt_hat latest sa =
    let rtt_error   = p_hat *. (Int64.to_float @@ error_of sa rtt_hat) in
    let age         = p_hat *. Int64.to_float (delta_TSC (fst latest).timestamps.tf (fst sa).timestamps.tf) in
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
                                            | false -> (subset_rtt,    None)
                                            | true  -> (subsubset_rtt, Some shift_subset))    (* Upwards shift detected *)
    <$>  subsubset_rtt <*> subset_rtt

let handle_RTT_upshift subsubset_rtt samples subset =
    let (left, right) = subset in
    slice_map samples left right (fun x -> (fst x, Some subsubset_rtt))

let normal_pstamp   subset =             snd <$> (min_and_where rtt_of subset)    (* returns a Fixed *)

(* NORMAL SUBSETS *)
 


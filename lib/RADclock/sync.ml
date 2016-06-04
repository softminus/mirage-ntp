(* sync stuff *)

open Types
open History

(* SHARED *)

let run_estimator_2win  estimator wins =
    match wins with
    | (Full a, Full b) -> estimator a b
    | (_     , _     ) -> failwith "invalid windows passed"

let run_estimator_1win  estimator win =
    match win with
    | Full a            -> estimator a
    | _                 -> failwith "invalid windows passed"

let point_of_history hi = (* takes a single-element history and gives us the point inside *)
    match (length hi) with
    | 1     -> nth hi 0
    | _     -> failwith "invalid list passed to point_of_se_history, this should never happen"

let rtt_of sample =
    let ts = sample.timestamps in
    let del = Int64.sub (ts.tf) (ts.ta) in
    match (del > 0L) with
    | true  -> del
    | false -> failwith "invalid RTT / causality error. This is a bug"

let error_of packet rtt_hat =
    delta_TSC (rtt_of packet) rtt_hat


let rate_of_pair newer_sample older_sample =
    let newer = newer_sample.timestamps in
    let older = older_sample.timestamps in
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
let warmup_pstamp   win = Some           (snd @@ min_and_where rtt_of win)
let warmup_rtt_hat  win = Some (rtt_of @@ fst @@ min_and_where rtt_of win)


let warmup_p_hat rtt_hat near far =
    let best_in_near    = fst @@ min_and_where rtt_of near  in
    let best_in_far     = fst @@ min_and_where rtt_of far   in
    let p_hat = rate_of_pair best_in_near best_in_far in
    match p_hat with
    | None -> None
    | Some p ->
            let del_tb      = best_in_near.timestamps.tb -. best_in_far.timestamps.tb in
            let far_error   = Int64.to_float @@ error_of best_in_far  rtt_hat in
            let near_error  = Int64.to_float @@ error_of best_in_near rtt_hat in
            let p_hat_error = (p /. del_tb) *. (far_error +. near_error) in
            Some (p, p_hat_error)

let warmup_C_fixup latest old_C old_p_hat new_p_hat =
    let newest = point_of_history latest in
    Some (old_C +. (Int64.to_float newest.timestamps.ta) *. Int64.to_float (Int64.sub old_p_hat new_p_hat))

let warmup_p_local = warmup_p_hat






(* WARMUP WINDOWS *)

let win_warmup_pstamp   ts =    (* FOR: warmup_pstamp *)
    range_of ts Newest Oldest

let win_warmup_rtt_hat  ts =    (* FOR: warmup_rtt *)
    range_of ts Newest Oldest

let win_warmup_p_hat    ts =    (* FOR: warmup_p_hat *)
    let wwidth = 1 + (length ts) / 4 in
    let near    = range_of ts Newest @@ Older(Newest, wwidth - 1)                                       in
    let far     = range_of ts                                       (Newer(Oldest, wwidth - 1)) Oldest  in
    (near, far)

let win_warmup_C_fixup  ts =    (* FOR: warmup_C_fixup *)
    range_of ts Newest Newest

let win_warmup_p_local = win_warmup_p_hat



(* NORMAL ESTIMATORS *)
(* NORMAL WINDOWS *)


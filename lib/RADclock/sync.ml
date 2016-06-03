(* sync stuff *)

open Types
open History

(* SHARED *)
let run_estimator estimator win =
    match win with
    | (Full a, Full b) -> estimator a b
    | (_     , _     ) -> failwith "invalid windows passed"

let rtt_of sa =
    let del = Int64.sub (sa.tf) (sa.ta) in
    match (del > 0L) with
    | true  -> del
    | false -> failwith "invalid RTT / causality error. This is a bug"

let rate_of_pair newer older =
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
let warmup_pstamp_i win = Some           (snd @@ min_and_where rtt_of win)
let warmup_rtt_hat  win = Some (rtt_of @@ fst @@ min_and_where rtt_of win)


let warmup_p_hat rtt_hat near far =
    let best_in_near    = fst @@ min_and_where rtt_of near  in
    let best_in_far     = fst @@ min_and_where rtt_of far   in
    let p_hat = rate_of_pair best_in_near best_in_far in
    match p_hat with
    | None -> None
    | Some p ->
            let del_tb      = best_in_near.tb -. best_in_far.tb in
            let rtt_far     = Int64.to_float @@ delta_TSC (rtt_of best_in_far ) rtt_hat in
            let rtt_near    = Int64.to_float @@ delta_TSC (rtt_of best_in_near) rtt_hat in
            let p_hat_error = (p /. del_tb) *. (rtt_far +. rtt_near) in
            Some (p, p_hat_error)

let warmup_C_fixup latest old_C old_p_hat new_p_hat =
    old_C +. (Int64.to_float latest.ta) *. Int64.to_float (Int64.sub old_p_hat new_p_hat)

let warmup_p_local = warmup_p_hat






(* WARMUP WINDOWS *)

let win_warmup_rtt ts =     (* FOR: warmup_pstamp_i warmup_rtt *)
    ts

let win_warmup_p_hat ts =   (* FOR: warmup_p_hat *)
    let wwidth = 1 + (length ts) / 4 in
    let near    = range_of ts Newest @@ Older(Newest, wwidth - 1)                                       in
    let far     = range_of ts                                       (Newer(Oldest, wwidth - 1)) Oldest  in
    (near, far)

let win_warmup_C_fixup ts = (* FOR: warmup_C_fixup *)
    get ts Newest

let win_warmup_p_local = win_warmup_p_hat



(* NORMAL ESTIMATORS *)
(* NORMAL WINDOWS *)


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
    | true  -> Some del
    | false -> None

let rate_of_pair newer older =
    let delTa = Int64.sub newer.ta  older.ta in
    let delTb = newer.tb -.         older.tb in
    let delTe = newer.te -.         older.te in
    let delTf = Int64.sub newer.tf  older.tf in

    match (delTa > 0L, delTb > 0.0, delTe > 0.0, delTf > 0L) with
    | (true,true, true, true) ->
            let forwards    = delTb /. (Int64.to_float delTa) in
            let reverse     = delTe /. (Int64.to_float delTf) in
            (forwards +. reverse) /. 2.0
    | (_,   _,    _,    _   ) -> failwith "invalid timestamps provided"



(* WARMUP ESTIMATORS *)
let warmup_pstamp_i win =           snd @@ min_and_where rtt_of win
let warmup_rtt      win = rtt_of @@ fst @@ min_and_where rtt_of win


let warmup_p_hat near far =
    let best_in_near = snd

(* WARMUP WINDOWS *)

let warmup_rtt_window ts =      (* FOR: warmup_pstamp_i warmup_rtt *)
    ts

let warmup_p_hat_windows ts =   (* FOR: warmup_p_hat *)
    let wwidth = 1 + (length ts) / 4 in
    let near    = range_of ts Newest @@ Older(Newest, wwidth - 1)                                       in
    let far     = range_of ts                                       (Newer(Oldest, wwidth - 1)) Oldest  in
    (near, far)

(* NORMAL ESTIMATORS *)
(* NORMAL WINDOWS *)


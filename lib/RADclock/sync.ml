(* sync stuff *)

open Types
open History

(* SHARED *)
let rtt_of sa =
    let del = Int64.sub (sa.tf) (sa.ta) in
    match (del > 0L) with
    | true  -> del
    | false -> failwith "invalid TSC values for sample"

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

(* WARMUP *)
let warmup_pstamp_i sample_list =           snd @@ min_and_where rtt_of sample_list
let warmup_rtt      sample_list = rtt_of @@ fst @@ min_and_where rtt_of sample_list

(* FIXME define a set of functions that generates all the intervals/windows
 * ever used in terms of the current intervals and if an update generated
 * correct data -- in a way that doesn't depend on any of the actual data. make
 * sure they always remain valid regardless of if things like rtt_of fail to
 * produce a result.
 *)

let warmup_rtt_windows 
(* NORMAL *)


(* let step_state state sa =
    match state.regime with
    | ZERO  *)

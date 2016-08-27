open Types
open Util
open History

let check_causalities prospective extant =
    let newer = prospective.timestamps in
    let older =      extant.timestamps in

    let del_intra   = Int64.sub newer.tf newer.ta in

    let delTa       = Int64.sub newer.ta older.ta in
    let delTf       = Int64.sub newer.tf older.tf in

    match (del_intra > 0L, delTa > 0L, delTf > 0L) with
    | (true, true, true)    -> Some prospective
    | (_   , _   , _   )    -> None

let delta_TSC newer older =
    let del = Int64.sub newer older in
    match (del >= 0L) with
    | true  -> del
    | false -> failwith (Printf.sprintf "invalid ΔTSC! newer = %Lx, older= %Lx" newer older)

let baseline newer older =
    delta_TSC (fst newer).timestamps.tf (fst older).timestamps.tf

let rtt_of_prime sample =
    let ts = sample.timestamps in
    let del = Int64.sub (ts.tf) (ts.ta) in
    match (del > 0L) with
    | true  -> del
    | false -> failwith "invalid RTT / causality error. This is a bug"
    (* we are confident leaving this as an exception and returning a Maybe because the only way it can be reached is if the
     * TSC counter counts backwards or the code somehow invoked it in a bad way. There is no way that data from the network
     * can cause this exception to be reached, only a devastating bug in the code or the TSC counter being used violating its
     * invariants
     *)

let rtt_of sample =
    rtt_of_prime (fst sample)

let check_positive x =
    match (x > 0.0) with
    | true -> x
    | false -> failwith "should be positive!"

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

let plocal_theta_of         p_hat c p_local latest  sample =
    let theta   =  theta_of p_hat c                 sample in

    match p_local with
    | None          -> theta
    | Some p_local  -> theta -. (p_local -. p_hat) *. Int64.to_float (baseline latest sample)

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

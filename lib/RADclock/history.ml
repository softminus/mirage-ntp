(* corresponds to sync_history.[ch] in RADclock *)

(* this is equivalent to RADclock's sync_hist type (in sync_history.h) and
 * history{_init,_free,_add, _end,_find,_resize,_min,_min_slide,_min_slide_value}
 * (in sync_history.c).
 *
 * The most recent items are at the head of the list, the oldest are at the tail.
 * When we add a sample, we add it at the head and drop a tail element if necessary
 *
 *)

type 'a history = History of int * int * 'a list (* History (length, fixup, List) *)

type point = Now | Ago of point * int | Fixed of int

type range = Range of point * point

type status = Ready of int | NotReady of int | NeverReady


let rec init_aux a b =
    match (a, b) with
    | (_, [])       ->  []
    | (y, z::zs)    ->  y :: init_aux z zs

let init = function
    []      ->  failwith "init"
  | x::xs   ->  init_aux x xs

let rec take_aux n li =
    match (n, li) with
    | (_, [])       ->  []
    | (1, z::zs)    ->  [z]
    | (n, z::zs)    ->  z :: take_aux (n - 1) zs

let take n li =
    match (n > 0) with
    | true  ->  take_aux n li
    | false ->  []

let rec idx_of_point h p =
    match p with
    | Now           ->  0
    | Ago (z, zd)   ->  idx_of_point h z + zd
    | Fixed (i)     ->
            match h with History(sz, fixup, l) ->
                fixup + i




let nth h n =
    match h with History(sz, fixup, l) ->
        List.nth l n

let hcons a h =
    match h with History(sz, fixup, l) ->
        match (sz > List.length(l)) with
        | true -> History(sz, fixup, a :: l)
        | false -> History (sz, fixup + 1, a :: (init l))

let resize h nsz =
    match h with History(sz, fixup, l) ->
        History(nsz, fixup, take nsz l)


let min_by extractor hist =
    let cmp x y = if (compare (extractor x) (extractor y) ) > 0 then y else x
    in
    match hist with History(sz, fixup, l) ->
        match l with
        | z::zs -> List.fold_left cmp z zs
        | [] -> failwith "min_by"





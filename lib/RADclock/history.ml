(* corresponds to sync_history.[ch] in RADclock *)

(* this is equivalent to RADclock's sync_hist type (in sync_history.h) and
 * history{_init,_free,_add, _end,_find,_resize,_min,_min_slide,_min_slide_value}
 * (in sync_history.c).
 *
 * The most recent items are at the head of the list, the oldest are at the tail.
 * When we add a sample, we add it at the head and drop a tail element if necessary
 *
 *          _L*           _R*                        Oldest sample
 *         /             /                           |
 *         V             V                           V
 *         *      *      *
 *  H------G------F------E------D------C------B------A
 *                       #      #      #      #
 *  ^                    ^                    ^
 *  |                    |                    |
 *  Now                  \_L#                 \_R#
 *
 * Windows defined between two points include both points.
 *
 *
 * FIXME: write quickcheck tests for this
 *
 *
 *)

open Util

type 'a history = History of int * int * 'a list (* History (length, fixup, List) *)

type point = Now | Ago of point * int | Fixed of int

type range = Range of point * point

type status = Ready of int | NotReady of int | NeverReady


let rec idx_of_point h p =
    match p with
    | Now           ->  0
    | Ago (z, zd)   ->  idx_of_point h z + zd
    | Fixed (i)     ->
            match h with History(sz, fixup, l) ->
                fixup + i

let rawlist h =
    match h with History(sz, fizup, l) -> l

let length h =
    List.length @@ rawlist h

let nth h n =
    match h with History(sz, fixup, l) ->
        List.nth l n

let valid_point h p =
    length h > idx_of_point h p

let at h p =
    match valid_point h p with
    | true  -> Some (nth h @@ idx_of_point h p)
    | false -> None

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

(* let range_of hist left right = *)



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

type 'a history = History of int * int * 'a list (* History (capacity, fixup, List) *)

type point = Now | Ago of point * int | Fixed of int

type validity = Valid | NotReady | Invalid

type range = Range of point * point

type range_status = Ready of int | NotReady of int | NeverReady


let rec idx_of_point h p =
    match p with
    | Now           ->  0
    | Ago (z, zd)   ->  idx_of_point h z + zd
    | Fixed (i)     ->
            match h with History(cap, fixup, l) ->
                fixup + i

let rawlist h =
    match h with History(cap, fixup, l) -> l

let capacity h =
    match h with History(cap, fixup, l) -> cap

let length h =
    List.length @@ rawlist h

let nth h n =
    List.nth (rawlist h) n

let validity h p =
    match (length h > idx_of_point h p) with
    | true  -> Valid
    | false -> match (

let at h p =
    match validity h p with
    | Valid -> Some (nth h @@ idx_of_point h p)
    | _     -> None

let hcons a h =
    match h with History(cap, fixup, l) ->
        match (cap > List.length(l)) with
        | true -> History(cap, fixup, a :: l)
        | false -> History (cap, fixup + 1, a :: (init l))

let resize h ncap =
    match h with History(cap, fixup, l) ->
        History(ncap, fixup, take ncap l)


let min_by extractor hist =
    let cmp x y = if (compare (extractor x) (extractor y) ) > 0 then y else x
    in
    match hist with History(cap, fixup, l) ->
        match l with
        | z::zs -> List.fold_left cmp z zs
        | [] -> failwith "min_by"

(* let range_of hist left right = *)



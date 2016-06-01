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
 *         V             V       right -->           V
 *         *      *      *
 *  H------G------F------E------D------C------B------A
 *                       #      #      #      #
 *  ^     <--left        ^                    ^
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

type 'a history = History of int * int * 'a list (* History (capacity, offset, List) *)

type point = Now | Ago of point * int | Fixed of int * int (* Fixed (index, oldoffset) *)

type point_validity = Valid | NotReady | Invalid

type range_spec = Range of point * point

type 'a range = Full of 'a history | NotFull | InvalidEdges


let rec idx_of_point h p =
    match p with
    | Now           ->  0
    | Ago (z, zd)   ->  idx_of_point h z + zd
    | Fixed (idx, oldoffset)     ->
            match h with History(cap, offset, l) ->
                idx + (offset - oldoffset)

let fixed_of_point h p =
    let idx = idx_of_point h p in
    match h with History(cap, offset, l) ->
        Fixed(idx, offset)

let rawlist h =
    match h with History (cap, offset, l) -> l

let capacity h =
    match h with History (cap, offset, l) -> cap

let length h =
    List.length @@ rawlist h

let nth h n =
    List.nth (rawlist h) n

let validity h p =
    if idx_of_point h p < 0 then invalid_arg "validity" else
    match (length h > idx_of_point h p) with
    | true  -> Valid
    | false -> match (capacity h > idx_of_point h p) with
        | true  -> NotReady
        | false -> Invalid

let get h p =
    match validity h p with
    | Valid -> Some (nth h @@ idx_of_point h p)
    | _     -> None

let hcons a h =
    match h with History (cap, offset, l) ->
        match (cap > List.length(l)) with
        | true  -> History (cap, offset + 1, a :: l)
        | false -> History (cap, offset + 1, a :: (init l))

let rec synth_aux element h n =
    match n with
    | 1     -> hcons element h
    | n     -> synth_aux element (hcons element h) (n-1)

let synthetic element h n =
    match (0 < n) with
    | false -> invalid_arg "synthetic"
    | true -> synth_aux element h n

let resize h ncap =
    match h with History (cap, offset, l) ->
        History (ncap, offset, take ncap l)


let min_by extractor hist =
    let cmp x y = if (compare (extractor x) (extractor y) ) > 0 then y else x
    in
    match hist with History (cap, offset, l) ->
        match l with
        | z::zs -> List.fold_left cmp z zs
        | [] -> failwith "min_by"

let range_slice hist left right =
    let idx_l = idx_of_point hist left in
    let idx_r = idx_of_point hist right in
    match hist with History (cap, offset, l) ->
        let head_taken_off = drop idx_l l in
        let slice = take (idx_r - idx_l + 1) head_taken_off in
        let slice_offset = offset - idx_l in
        let capacity = List.length slice in
        History (capacity, slice_offset, slice)

let slice_map hist left right f =
    let idx_l = idx_of_point hist left in
    let idx_r = idx_of_point hist right in
    match hist with History (cap, offset, l) ->
        let left_untouched = take idx_l l in
        let right_untouched = drop (idx_r + 1) l in
        let head_taken_off = drop idx_l l in
        let slice = take (idx_r - idx_l + 1) head_taken_off in
        History (cap, offset, left_untouched @ (List.map f slice) @ right_untouched)


let range_of hist left right =
    match (validity hist left, validity hist right) with
    | (Invalid,     _)          -> InvalidEdges
    | (_,           Invalid)    -> InvalidEdges
    | (NotReady,    _)          -> NotFull
    | (_,           NotReady)   -> NotFull
    | (Valid,       Valid)          ->
            match (idx_of_point hist left < idx_of_point hist right) with
            | false -> invalid_arg "range ordering invalid"
            | true  -> Full (range_slice hist left right)

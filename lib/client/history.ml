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
 *  Newest               \_L#                 \_R#
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
[@@deriving show]

type point = Newest | Oldest | Full_last | Older of point * int | Newer of point * int | Fixed of int * int (* Fixed (index, oldoffset) *)
[@@deriving show]

type point_validity = Valid | NotReady | Invalid

type range_spec = Range of point * point

type 'a range = Full of 'a history | NotFull | InvalidEdges

let rawlist h =
    match h with History (cap, offset, l) -> l

let capacity h =
    match h with History (cap, offset, l) -> cap

let length h =
    List.length @@ rawlist h

let nth h n =
    List.nth (rawlist h) n

let rec idx_of_point h p =
    match p with
    | Newest                    ->  0
    | Older (z, zd)             ->  idx_of_point h z + zd
    | Newer (z, zd)             ->  idx_of_point h z - zd
    | Oldest                    ->  length h - 1
    | Full_last                 ->  capacity h - 1
    | Fixed (idx, oldoffset)    ->  match h with History(cap, offset, l) -> idx + (offset - oldoffset)

let fixed_of_point h p =
    let idx = idx_of_point h p in
    match h with History(cap, offset, l) ->
        Fixed(idx, offset)


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

let get_sure h p =
    match validity h p with
    | Valid -> (nth h @@ idx_of_point h p)
    | _     -> invalid_arg "invalid point!"

let hcons a h =
    match h with History (cap, offset, l) ->
        match (cap > List.length(l)) with
        | true  -> History (cap, offset + 1, a :: l)
        | false -> History (cap, offset + 1, a :: (init l))

let tl h =
    match h with History (cap, offset, l) ->
        match (List.length(l) > 0) with
        | false -> failwith "history.tl"
        | true  -> History (cap, offset - 1, List.tl l)

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

let map hist f =
    match hist with History (cap, offset, l) ->
        History(cap, offset, List.map f l)

let fold hist f x0 =
    List.fold_left f x0 @@ rawlist hist

(* returns (Σ f * weight, Σ weight)  *)
let weighted_sum f weight hist =
    let acc f w (sum, norm) sample =
        let value   = f sample in
        let weight  = w sample in
        let weighted = value *. weight in
        (sum +. weighted, norm +. weight)
    in
    fold hist (acc f weight) (0.0, 0.0)

let rec min_where_aux extractor li =
    match li with
    | []    -> failwith "min_where_aux consistency failure"
    | [x]   -> (0, x)
    | x::xs ->
            let pz, z = min_where_aux extractor xs in
            match (compare (extractor z) (extractor x) < 0) with
            | true  -> (pz + 1, z)
            | false -> (0,      x)

(* min_and_where returns an *element* of the list, without the extractor applied to it *)
let min_and_where extractor hist =
    match hist with History (cap, offset, l) ->
        match l with
        | [] -> None
        | z ->
                let rv = min_where_aux extractor z in
                let idx = fst rv in
                let x   = snd rv in
                Some (x, Fixed(idx, offset))


let range_of hist left right =
    match (validity hist left, validity hist right) with
    | (NotReady,    NotReady)   -> None
    | (Valid,       Valid   )   -> (match (idx_of_point hist left <= idx_of_point hist right) with
                                    | true  -> Some (range_slice hist left right)
                                    | false -> invalid_arg "range ordering invalid")
    | (_,           _       )   -> invalid_arg "invalid points!"


let valid h p =
    match validity h p with
    | Invalid   -> false
    | NotReady  -> true
    | Valid     -> true

let valid_range h l r =
    match (valid h l, valid h r) with
    | (true, true)  -> (idx_of_point h l <= idx_of_point h r)
    | _             -> false

let valid_pair hist         left_1 right_1                       left_2 right_2 =
    match (valid_range hist left_1 right_1,     valid_range hist left_2 right_2) with
    | (true, true)  -> true
    | _             -> false

let order hist x y =                (* returns (lefter point, righter point) *)
    match (idx_of_point hist x <= idx_of_point hist y) with
    | true  -> (x, y)
    | false -> (y, x)

let intersect_range hist left_1 right_1 left_2 right_2 =
    match (valid_pair hist left_1 right_1 left_2 right_2) with
    | false -> invalid_arg "invalid arguments provided!"
    | true  ->
            let lcan = snd @@ (order hist left_1  left_2 ) in
            let rcan = fst @@ (order hist right_1 right_2) in
            match (valid_range hist lcan rcan) with
            | true  -> (lcan, rcan)
            | false -> invalid_arg "non-overlapping lists!"

let range_length hist left right =
    match valid_range hist left right with
    | false -> None
    | true  -> Some (idx_of_point hist right - idx_of_point hist left + 1)

let point_of_history hi = (* takes a single-element history and gives us the point inside *)
    match (length hi) with
    | 1     -> nth hi 0
    | _     -> failwith "invalid list passed to point_of_se_history, this should never happen"

let point_of_range ra =
    match ra with
    | Full hi -> point_of_history hi
    | _     -> failwith "point_of_range argument error"

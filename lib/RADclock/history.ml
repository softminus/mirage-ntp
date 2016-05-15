(* corresponds to sync_history.[ch] in RADclock *)

(* this is equivalent to RADclock's sync_hist type (in sync_history.h) and
 * history{_init,_free,_add, _end,_find,_resize,_min,_min_slide,_min_slide_value}
 * (in sync_history.c).
 *
 * The most recent items are at the head of the list.
 *
 *)

type 'a history = History of int * 'a list



let rec init_aux a b =
    match (a, b) with
    | (_, [])       ->  []
    | (y, z::zs)    ->  y :: init_aux z zs

let init = function
    []      ->  failwith "init"
  | x::xs   ->  init_aux x xs








let hcons a h =
    match h with History(sz, l) ->
        match (sz > List.length(l)) with
        | true -> History(sz, a :: l)
        | false -> History (0,[3])

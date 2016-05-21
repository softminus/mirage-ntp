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

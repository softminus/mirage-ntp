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
    match (0 < n) with
    | true  ->  take_aux n li
    | false ->  []

let rec drop_aux n li =
    match (n, li) with
    | (_, [])       -> []
    | (1, _::zs)    -> zs
    | (n, _::zs)    -> drop_aux (n -1) zs

let drop n li =
    match (n > 0) with
    | true  ->  drop_aux n li
    | false ->  li

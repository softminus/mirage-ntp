let (>>=) x f =                 (* bind *)
    match x with
    | Some x    -> (f x)
    | None      -> None

let (<$>) f m =                 (* fmap *)
    match m with
     | Some x   -> Some (f x)
     | None     -> None

let (<*>) f m =                 (* ap *)
    match f with
    | Some ff   -> ff <$> m
    | None      -> None

let (<|>) l r =
    match l with
    | None      -> r
    | x         -> x

let join x = x >>= (fun x -> x)

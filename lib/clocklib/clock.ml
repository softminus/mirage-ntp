open Types

let absolute_time alg_out tsc =
    let fixup_local p_hat est =
        match est.p_local with
        | Some (p_local, _) -> p_local -. p_hat
        | None              -> 0.0
    in
    match (alg_out.freshness, alg_out.p_hat_and_error, alg_out.ca_and_error) with
    | (last_tsc, (p_hat, _), (ca, _))   -> Some ((Int64.to_float tsc) *. p_hat +. ca +. (Int64.to_float (Int64.sub tsc last_tsc)) *. fixup_local p_hat alg_out)
    | _                                 -> None

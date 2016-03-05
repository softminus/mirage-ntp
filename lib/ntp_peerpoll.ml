open Ntp_wire
open Ntp_types

let validate_packet b ctx =
    let pkt = pkt_of_buf b in
    match pkt with
    | None -> None
    | Some p ->
            if p.version    <>  4                       then None else

            if p.trans_ts   =   int64_to_ts Int64.zero  then None else (* server not sync'd *)
            if p.recv_ts    =   int64_to_ts Int64.zero  then None else (* server not sync'd *)

            if p.trans_ts   =   ctx.recv.fe_transmit    then None else (* we already saw this packet *)
            if p.origin_ts  <>  ctx.send.ne_transmit    then None else (* this packet doesn't have the timestamp
                                                                          we struck in it *)
            Some p





(*
 *      2*offset    = (fe_receive - ne_transmit) + (fe_transmit - ne_receive)
 *)

let p_offset        p ctx = ((delta_ts p.recv_ts          ctx.send.ne_transmit) +.
                             (delta_ts p.trans_ts         ctx.recv.ne_receive )) /. 2.0
(*
 *      delay       = (ne_receive - ne_transmit) - (fe_transmit - fe_receive)
 *
 * note that if the network delay is small and our clock's frequency is too slow the server will
 * have counted off more seconds than we have and so the calculated delay will be negative, so we
 * clamp the output of p_delay to rho.
 *)
let p_delay         p ctx = max (log_to_float Ntp_constants.rho)
                            ((delta_ts ctx.recv.ne_receive ctx.send.ne_transmit)
                          -. (delta_ts p.trans_ts          p.recv_ts           ))

let p_dispersion    p ctx = log_to_float p.precision                (* the precision of their clock *)
                            +. log_to_float Ntp_constants.rho       (* and that of ours *)
                            (* and the dispersion error, based on the time between the two timestamps we strike *)
                            +. Ntp_constants.phi *. (delta_ts ctx.recv.ne_receive ctx.send.ne_transmit)






(* this *only* returns a new filter list and does not calculate anything beyond what's needed
 * to do so.
 *
 * to calculate new filter list we remove the oldest sample, add the newest, and update all the dispersion
 * values. the dispersion/error of a value measured at t_0 is given by:
 *
     * dispersion(t) = p_dispersion + phi * (t - t_0)
 *
 * so each dispersion value stored in the filter list is incremented by phi for each second it's been
 * staying in the filter list
 *)

let updated_filter time filter sample =
    let updated_sample t s = match (s.dispersion, s.ne_recv, t) with (Ntp_wire.Seconds disp, Ntp_wire.Span recv, Ntp_wire.Span current_time) ->
        {s with total_dispersion = Ntp_wire.Seconds (disp +. Ntp_constants.phi *. (Int64.to_float current_time -. Int64.to_float recv))}
    in
    let updated = List.map (updated_sample time) filter in
    let with_new = [sample] @ List.rev (List.tl (List.rev updated)) in
    with_new


(* to select a sample from the filter, we look at the sample with the *lowest delay*
 * that is still in the filter. If we've used it before, we return None (as it's not OK
 * to reuse samples -- it must be fresh)
 *)

let sorted_filter filter =
    let newer a b = match (a.delay, b.delay) with (Ntp_wire.Seconds a_delay, Ntp_wire.Seconds b_delay) -> compare a_delay b_delay
    in
    List.sort newer filter

let fresh_or_not time sample =
    match sample.ne_recv with (Ntp_wire.Span sample_time) ->
        match (sample_time > time) with
        | true -> Some sample
        | false -> None



(* generate dispersion and jitter statistics *)



let getDispersion sample=
    match sample.total_dispersion with Ntp_wire.Seconds d -> d

let dispersion_of_filter filter =
    let dispersions = List.map getDispersion (sorted_filter filter) in
    let bias i s = s *. 2. ** (-.(1.0 +. float i)) in
    let scaled = List.mapi bias dispersions in
    let total = List.fold_left (fun a b -> a +. b) 0.0 scaled in
    total
    

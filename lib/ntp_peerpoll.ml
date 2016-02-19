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

let p_offset        p ctx = (Int64.to_float(delta_ts p.recv_ts          ctx.send.ne_transmit) +.
                    Int64.to_float(delta_ts p.trans_ts         ctx.recv.ne_receive )) /. 2.0
(*
 *      delay       = (ne_receive - ne_transmit) - (fe_transmit - fe_receive)
 *
 * note that if the network delay is small and our clock's frequency is too slow the server will
 * have counted off more seconds than we have and so the calculated delay will be negative, so we
 * clamp the output of p_delay to rho.
 *)
let p_delay         p ctx = max (log_to_float Ntp_constants.rho)
                            (Int64.to_float(delta_ts ctx.recv.ne_receive ctx.send.ne_transmit)
                          -. Int64.to_float(delta_ts p.trans_ts          p.recv_ts           ))

let p_dispersion    p ctx = log_to_float p.precision                (* the precision of their clock *)
                            +. log_to_float Ntp_constants.rho       (* and that of ours *)
                            (* and the dispersion error, based on the time between the two timestamps we strike *)
                            +. Ntp_constants.phi *. Int64.to_float(delta_ts ctx.recv.ne_receive ctx.send.ne_transmit)







let update_filter filter offset_i delay_i dispersion_i ne_recv_i =


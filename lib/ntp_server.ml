(* this is the NTP server implementation:
 * get a query packet, reply to it with all the right timestamps filled in
 *)

open Ntp_wire
open Ntp_clock

let reply_of_query p cs time =
    let leap =              cs.leap in
    let version =           4 in
    let mode =              Server in
    let stratum =           cs.stratum in
    let precision =         cs.precision in
    let root_delay =        cs.root_delay in
    let root_dispersion =   cs.root_dispersion in
    let refid =             cs.refid in
    let reference_ts =      cs.reftime in

    let poll = p.poll in

    let origin_ts = p.trans_ts in
    let recv_ts = time in 
    let trans_ts = time in

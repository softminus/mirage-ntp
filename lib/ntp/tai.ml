(* we need to:
 * 
 * infer when the next leap second transition is (from data in NTP packets and
 * with calendar math)
 *
 * convert time from the wire (which is in UTC) to a timescale that is
 * continuous (TAI or TAI + a constant offset) that we can feed to the maths
 *
 * be able to convert that timescale back to UTC for consumers
 *)

open Maybe
open Wire
open Ntp_types
open Tsc_clock

(* we need state to infer when the next leap is gonna happen because NTP
 * protocol fails to explicitly encode the time at which the next leap occurs *)

type leap_infer_state = {
    time_seen:          float;  (* the last time we've seen an packet with the same LI values *)
    how_many:           int;    (* how many times we've seen the same LI values in reply packets *)
    flavor:             leap_flavor; (* negative or positive leap *)
}

let leap_update_aux state packet =
    match (state.flavor = packet.private_data.leap) with
    | false -> None
    | true  -> Some {state with how_many = state.how_many + 1; time_seen = packet.timestamps.te}

    
let leap_detect pkt lis =
    match pkt.private_data.leap with
    | Unsync    -> None (* seeing an Unsync   packet forces reset of leap infer state *)
    | NoWarning -> None (* seeing a NoWarning packet forces reset of leap infer state *)



type leap_translate_ctx = {
    bandsize:           int64;
    tai_offset_before:  int;
    transition_time:    int64;
    tai_offset_after:   int;
}


let in_deadband width center value =
    let dist = Int64.abs @@ Int64.sub center value in
    match (dist < width) with
    | true ->  Some value
    | false -> None

(* not a function for all x -- thus we restrict its domain *)
let wire_to_continous context x =
    match (in_deadband context.bandsize context.transition_time x) with
    | None -> None
    | Some utc ->
        match (utc < context.transition_time) with
        | true  -> Some (Int64.add utc (Int64.of_int context.tai_offset_before),context.tai_offset_before)
        | false -> Some (Int64.add utc (Int64.of_int context.tai_offset_after), context.tai_offset_after)


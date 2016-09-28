type ntp_context 

val initial_state : ntp_context
val generate_query : int64 -> ntp_context -> Cstruct.t * ntp_context
val process_reply : int64 -> Cstruct.t option -> ntp_context -> ntp_context
val output_of_state : ntp_context -> Tsc_clock.output option


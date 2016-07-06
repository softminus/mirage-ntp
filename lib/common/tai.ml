(*
 * NTP time is in the discontinuous UTC timescale and thus has duplicate values
 * because of leap seconds. This makes raw NTP timestamps unfit for feeding
 * into a rate/offset estimation algorithm.
 *
 * The NTP protocol is flawed and does not include an explicit field for the
 * current TAI/UTC offset. Furthermore, there is no field in NTP packets that
 * tells us (in any timescale) when the next leap second event is scheduled.
 *
 * The absence of those fields makes it impossible to easily synthesize a
 * discontinuity-free timescale from the NTP timestamps.
 *
 * (IEEE 1588 / PTP expresses timestamps in TAI and also includes the current
 * UTC/TAI offset in each packet and include a binary flag warning of an
 * upcoming leap second event. Similarly, GPS, BeiDou, and Galileo all use
 * timescales with no leap seconds (effectively, TAI) and include the current
 * UTC offset (and the time of an upcoming leap second event) in the navigation
 * messages transmitted from the space vehicles. None of those systems ever
 * have had issues with leap seconds.)
 *
 * There is only a binary flag that indicates whether a leap second event is
 * coming -- the NTP client *must* either do complex calendar maths to find out
 * *when* the leap event is or (from an out-of-band source) have been provided
 * a copy of an up-to-date leap second table.
 *
 * It is difficult to come up with a more pointlessly fragile, baroque, and
 * overly complex system for coping with leap second events.
 *
 *
 * Unfortunately, we are required to implement this logic to have a correct NTP
 * client.
 *
 *
 * We check to see if we have a valid and up-to-date leap table -- if so, we
 * know the current TAI offset, the time of the next leap second event, and the
 * TAI offset after the leap event. We can thus use it to unbake UTC timestamps
 * into TAI timestamps that can be used for rate/offset estimation (and bake in
 * the TAI offset as desired for consumers who want UTC time).
 *
 * If we do not have an up-to-date leap table we:
     * don't know the current TAI offset
     * we don't know the time of the leap second event
 *
 * The only information that NTP packets contain about leap second events is a
 * binary flag that indicates an upcoming leap second at the next UTC midnight.
 * This means we need to manually calculate when the next UTC midnight is, and
 * proceed accordingly.
 *
 * We also need to verify that the upcoming UTC midnight is the end of 30 June
 * or 31 December because some NTP servers set the leap flag prematurely -- see
 * doi://10.1007/978-3-319-30505-9_29. This is where the requirement for
 * calendar arithmetic comes in.
 *
 * All of this would be immaterial if NTP unambiguously encoded the current
 * TAI time and the current TAI/UTC offset, along with the TAI time of any
 * upcoming leap second event and the TAI/UTC offset after leap second event
 * -- like GPS. Most NTP servers get time information from GPS so this would
 * not even be a difficult change.
 *
 *
 * information flow if we have a valid leap second table:
 *
 *                          NTP timestamps (UTC)
 *                          |
 *                          |
 *                          V
 *        leap table----> unbake
 *                          |
 *                          |
 *                          T
 *                          A
 *                          I
 *                          |      timestamp counter-----\                  leap table
 *                          |                            |                      |
 *                          |                            |                      |
 *                          V                            V                      V
 *                      rate/offset estimation ------> client ----TAI---> synthesize_UTC
 *                                                                              |
 *                                                                              |
 *                                                                              V
 *                                                                             UTC
 *)


type tai = Int64
type utc = Int64
type unbaked = int64 * int64 (* timestamp in TAIlike scale, amount to add to it to get back UTC time *)

type leap_event = {
    transition_time:    tai;
    tai_offset_after:   int;
}

type leap_table = {
    end_of_validity:    utc;
    events:             leap_event list;
}

type synthetic_leap = {
    transition_time:    tai;        (* this is not actually TAI, but blame the NTP people *)
    offset_after:       int;
}

let run_table table idx =


let unbake_utc table ntp_timestamp =
    match (table.end_of_validity > ntp_timestamp) with
    | true

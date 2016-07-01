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
 * UTC/TAI offset in each packet. Similarly, GPS, BeiDou, and Galileo all use
 * timescales with no leap seconds (effectively, TAI) and include the current
 * UTC offset (and the time of an upcoming leap second event) in the navigation
 * messages transmitted from the space vehicles. None of those systems ever
 * have had issues with leap seconds.)
 *
 * There is only a binary flag that indicates whether a leap second event is
 * coming -- the NTP client *must* either do complex calendar maths to find out
 * *when* the leap event is or (from an out-of-band source) have been provided
 * a copy of an up-to-date leap second table. It is difficult to come up with a
 * more pointlessly fragile, baroque, and overly complex system for coping with
 * leap second events. 
 *
 * 
 *
 * Unfortunately, we are required to implement this logic to have a correct NTP
 * client.
 *)

type tai = Int64
type utc = Int64

type leap_event = {
    transition_time:    tai;
    tai_offset_after:   int;
}

type leap_table = {
    end_of_validity:    utc;
    events:             leap_event list;
    }
    
let tai_to_utc = 0

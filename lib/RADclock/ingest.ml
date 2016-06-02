open History
open Wire
open Types






let sample_of_packet history txt (pkt : pkt) rxt =
    let l = get history Newest in
    let quality = match l with
    | None -> OK
    | Some last ->
            (* FIXME: check for TTL changes when we have a way to get ttl of 
             * received packet from mirage UDP stack 
             *)
            if pkt.leap     <> last.leap    then NG else
            if pkt.refid    <> last.refid   then NG else
            if pkt.stratum  <> last.stratum then NG else OK
    in
    let ttl         = 64 in
    let stratum     = pkt.stratum in
    let leap        = pkt.leap in
    let refid       = pkt.refid in
    let rootdelay   = short_ts_to_float pkt.root_delay in
    let rootdisp    = short_ts_to_float pkt.root_dispersion in
    let stamp       = {ta = txt; tb = to_float pkt.recv_ts; te = to_float pkt.trans_ts; tf = rxt} in
    {quality; ttl; stratum; leap; refid; rootdelay; rootdisp; stamp}



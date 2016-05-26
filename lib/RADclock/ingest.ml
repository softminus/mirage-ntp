open History
open Wire
open Types






let ingest_packet history txt pkt rxt =
    let l = get history Now in
    let status = match l with
    | None -> OK
    | Some last ->
            (* FIXME: check for TTL changes when we have a way to get ttl of 
             * received packet from mirage UDP stack 
             *)
            if pkt.leap     <> last.leap    then NG else
            if pkt.refid    <> last.refid   then NG else
            if pkt.stratum  <> last.stratum then NG else OK
    in
    status

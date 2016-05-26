open Lwt
open Tsc
open Wire
open Ntp_client
open Int64

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
    module U = S.UDPV4

  let start c s =
    S.listen_udpv4 s ~port:123 (fun ~src ~dst ~src_port data ->
        C.log_s c (green "new UDP frame from %s %d"
                     (Ipaddr.V4.to_string src) src_port)
        >>= fun () ->
            let tsc_ts:ts = {seconds = Int32.of_int 3673250845; fraction = Int32.of_int 0} in
            let udp = S.udpv4 s in
            let pkt = pkt_of_buf data in
            match pkt with
        | None -> C.log_s c (yellow "fail")
        | Some f -> U.write ~source_port:123 ~dest_ip:src ~dest_port:123 udp (buf_of_pkt(new_reply tsc_ts tsc_ts f tsc_ts))
                



      );

    S.listen s

end

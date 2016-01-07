open Lwt
open Ntp_wire

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  let start c s =
    S.listen_udpv4 s ~port:5353 (fun ~src ~dst ~src_port data ->
        C.log_s c (green "new UDP frame from %s %d"
                     (Ipaddr.V4.to_string src) src_port)
        >>
        C.log_s c (yellow "data: %d\n" (Cstruct.len data))
        >>
        let pkt = pkt_of_buf data in
        match pkt with
        | None -> C.log_s c (yellow "fail")
        | Some f -> C.log_s c (green "works")
        >>
        Lwt.return_unit
      );

    S.listen s

end

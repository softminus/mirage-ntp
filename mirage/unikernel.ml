open Lwt
open Tsc
open Wire
open Ntp_client
open Int64

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let server = Ipaddr.V4.of_string_exn  "131.215.239.14"

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
    module U = S.UDPV4

  let start c s =
      let udp = S.udpv4 s in
      let no = Int64.of_int @@ Tsc.rdtsc() in
      let txts:ts = int64_to_ts no in
      U.write ~source_port:123 ~dest_ip:server ~dest_port:123 udp (buf_of_pkt (new_query txts));


end

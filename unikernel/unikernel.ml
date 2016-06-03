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


    let connect_to_server stack server =
        let udp = S.udpv4 stack in
        let st, push_st = Lwt_stream.create() in
        S.listen_udpv4 stack ~port:123 (fun ~src ~dst ~src_port data ->
            push_st @@ Some data;
            Lwt.return ()
            );
            st
    let rx stream =
        Lwt_stream.get stream
        >>= function
            | None -> Lwt.fail (Failure "heck")
            | Some buf -> Lwt.return buf

    let dump_packet c rxd txts =
        let packet = validate_packet rxd txts in
        match packet with
            | None -> C.log_s c (yellow "fail")
            | Some pkt -> C.log_s c (Printf.sprintf "recv %Lx\ntrans %Lx\ntime %Lx" (ts_to_int64 pkt.recv_ts) (ts_to_int64 pkt.trans_ts)(Int64.of_int @@ Tsc.rdtsc()))
        let start c s =
            let st = connect_to_server s server in
            let udp = S.udpv4 s in

            let nonce = Int64.of_int @@ Tsc.rdtsc() in
            let txts:ts = int64_to_ts nonce in
            C.log_s c (Printf.sprintf "send %Lx" (Int64.of_int @@ Tsc.rdtsc())) >>= fun () ->

            U.write ~source_port:123 ~dest_ip:server ~dest_port:123 udp (buf_of_pkt (new_query txts)) >>= fun () ->
            rx st >>= fun (rxd) ->
            dump_packet c rxd txts >>= fun () ->
            let nonce = Int64.of_int @@ Tsc.rdtsc() in
            let txts:ts = int64_to_ts nonce in
            C.log_s c (Printf.sprintf "send %Lx" (Int64.of_int @@ Tsc.rdtsc())) >>= fun () ->
            U.write ~source_port:123 ~dest_ip:server ~dest_port:123 udp (buf_of_pkt (new_query txts)) >>= fun () ->
            rx st >>= fun (rxd) ->
            dump_packet c rxd txts >>= fun () ->

            S.listen s



end

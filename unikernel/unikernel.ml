open Lwt
open Tsc
open Types
open Wire
open Int64
open Ntp_client


let server = Ipaddr.V4.of_string_exn  "131.215.239.14"

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
    module U = S.UDPV4


    let connect_to_server stack server =
        let udp = S.udpv4 stack in
        let st, push_st = Lwt_stream.create() in
        S.listen_udpv4 stack ~port:123 (fun ~src ~dst ~src_port data ->
            let ts = Int64.of_int @@ Tsc.rdtsc() in
            push_st @@ Some (data, ts);
            Lwt.return ()
            );
            st
    let rx stream =
        Lwt_stream.get stream
        >>= function
            | None -> Lwt.fail (Failure "heck")
            | Some buf -> Lwt.return buf

    let dump_packet c rxd nonce =
        let packet = validate_reply rxd nonce in
        match packet with
            | None -> C.log_s c ("fail")
            | Some pkt -> C.log_s c (Printf.sprintf "recv %Lx\ntrans %Lx\ntime %Lx" (ts_to_int64 pkt.recv_ts) (ts_to_int64 pkt.trans_ts)(Int64.of_int @@ Tsc.rdtsc()))
        let start c s =
            let st = connect_to_server s server in
            let udp = S.udpv4 s in

            let rec do_it state = function
                | 0 -> Lwt.return (state)
                | n -> (let q = new_query (Int64.of_int @@ Tsc.rdtsc()) in
                        C.log_s c (Printf.sprintf "send ONE %Lx" ((fst q).tsc)) >>= fun () ->
                        U.write ~source_port:123 ~dest_ip:server ~dest_port:123 udp (snd q) >>= fun () ->
                        rx st >>= fun (rxd) ->
                        C.log_s c (Printf.sprintf "recv ONE %Lx" (snd rxd)) >>= fun () ->
                        let state = add_sample state (fst rxd) (fst q) (snd rxd) in
                        let state = update_estimators state in
                        OS.Time.sleep 1.0 >>= fun () ->
                        C.log_s c (show_sync_state state) >>=fun() -> Lwt.return(state)) >>=fun (state) -> do_it state (n-1)
            in
            do_it blank_state 3000 >>=fun(state) ->

            S.listen s
end

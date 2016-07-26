open Mirage

let net = generic_stackv4 default_console tap0

let client =
    let libraries = ["wire" ;"tsc"; "ntp_client";"clocklib" ] in
    let packages = [  ] in
  foreign
    ~libraries ~packages ~deps:[abstract nocrypto]
    "Unikernel.Main" (console @-> stackv4 @-> job)

let () =
    register "network" [
        client $ default_console $ net
  ]

open Mirage

let stack = generic_stackv4 default_console tap0

let client =
    let libraries = ["wire" ;"tsc" ] in
  let packages = [  ] in
  foreign
    ~libraries ~packages
    "Unikernel.Main" @@ console @-> stackv4 @-> job

let () =
    register "network" [
        client $ default_console $ stack
  ]

(* this simulates a timer/counter that is driven by an oscillator with
 * realistic error characteristics *)

type counter = Cstruct.uint64 [@printer fun fmt -> fprintf fmt "0x%Lx"]

type tsc_params = {
    period:     float;
}

let tsc_of_time params time =
    Int64.of_float @@ params.period *. time

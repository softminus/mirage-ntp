(* here we store all the terrible constants *)


(* phi, in units of seconds/seconds, represents the maximum frequency error that
 * we assume exists on our clock. If we multiply it by how long ago we've taken a
 * measurement, it gives us the maximum error of that measurement due to our clock's
 * frequency error, termed "dispersion"
 *)
let phi = 15E-6

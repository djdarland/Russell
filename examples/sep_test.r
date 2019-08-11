(* Simple example of separate compilation facility. *)
(* Compile identity.r first.                        *)
let
    identity == extern { "identity" }
in
    put[identity[13]]; put["\n"]
ni

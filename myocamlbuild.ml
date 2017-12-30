open Ocamlbuild_plugin;;

flag ["link"; "ocaml"; "gcc"] (S[A"-cc"; A"gcc"]);;
dep ["link"; "ocaml"; "use_bindings"] ["bindings.o"];;

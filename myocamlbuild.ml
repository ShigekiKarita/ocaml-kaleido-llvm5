open Ocamlbuild_plugin;;

(* TODO: use C++ https://www.versti.eu/TranslateProxy/https/github.com/ygrek/ocaml-hypertable/blob/master/myocamlbuild.ml *)
flag ["link"; "ocaml"; "gcc"] (S[A"-cc"; A"gcc"]);;
dep ["link"; "ocaml"; "use_bindings"] ["bindings.o"];;

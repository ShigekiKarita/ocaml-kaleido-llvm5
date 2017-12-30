(*===----------------------------------------------------------------------===
 * Main driver code.
 *===----------------------------------------------------------------------===*)

open Batteries
module L = Llvm
module E = Llvm_executionengine
module O = Llvm_scalar_opts

let filename = ref None

let set_filename s = filename := Some s

let speclist = [
    ("-f", Arg.String set_filename, ": input filename")
  ]

let main () =
  Arg.parse speclist set_filename "usage";
  ignore (E.initialize ());

  (* Prime the first token. *)
  print_string "ready> "; flush stdout;

  (* Create the JIT. *)
  let the_execution_engine = E.create Codegen.the_module in
  let the_fpm = L.PassManager.create_function Codegen.the_module in

  (* Set up the optimizer pipeline. *)

  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  O.add_instruction_combination the_fpm;

  (* reassociate expressions. *)
  O.add_reassociation the_fpm;

  (* Eliminate Common SubExpressions. *)
  O.add_gvn the_fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  O.add_cfg_simplification the_fpm;

  ignore (L.PassManager.initialize the_fpm);

  (* Run the main "interpreter loop" now. *)
  let fname, input = match !filename with
    | None -> "<stdin>", Lexing.from_channel stdin
    | Some s -> s, Lexing.from_channel @@ open_in s in
  input.lex_curr_p <- { input.lex_curr_p with pos_fname = fname };
  Toplevel.main_loop the_fpm the_execution_engine input;

  (* Print out all the generated code. *)
  L.dump_module Codegen.the_module
;;

main ()

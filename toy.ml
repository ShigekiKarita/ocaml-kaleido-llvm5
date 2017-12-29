(*===----------------------------------------------------------------------===
 * Main driver code.
 *===----------------------------------------------------------------------===*)

open Llvm
(* open Llvm_executionengine *)
open Llvm_target
open Llvm_scalar_opts

let main () =
  ignore (Llvm_executionengine.initialize ());

  (* Prime the first token. *)
  print_string "ready> "; flush stdout;
  (* let stream = Lexer.lex (Stream.of_channel stdin) in *)

  (* Create the JIT. *)
  let the_execution_engine = Llvm_executionengine.create Codegen.the_module in
  let the_fpm = PassManager.create_function Codegen.the_module in

  (* Set up the optimizer pipeline. *)

  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  add_instruction_combination the_fpm;

  (* reassociate expressions. *)
  add_reassociation the_fpm;

  (* Eliminate Common SubExpressions. *)
  add_gvn the_fpm;

  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  add_cfg_simplification the_fpm;

  ignore (PassManager.initialize the_fpm);

  (* Run the main "interpreter loop" now. *)
  Toplevel.main_loop the_fpm the_execution_engine;

  (* Print out all the generated code. *)
  dump_module Codegen.the_module
;;

main ()

(*===----------------------------------------------------------------------===
 * Top-Level parsing and JIT Driver
 *===----------------------------------------------------------------------===*)

open Llvm
open Llvm_executionengine

let anonymous_func_count = ref 0

(* top ::= definition | external | expression | ';' *)
let rec main_loop the_fpm the_execution_engine =
  (* TODO support multiple expressions, externals and definitions *)
  let parsed = Parser.entry_point Lexer.token (Lexing.from_channel stdin) in
  begin
  match parsed with
  | None -> ()
  | Some result ->
     Printf.printf "AST: %s\n" Ast.(show result);
     begin match result with
     | Ast.Semicolon -> () (* ignore top-level semicolons. *)
     | Ast.Definition e ->
        begin
          print_endline "parsed a function definition.";
          dump_value (Codegen.codegen_func the_fpm e);
        end
     | Ast.Extern e ->
        begin
         print_endline "parsed an extern.";
         dump_value (Codegen.codegen_proto e);
        end
     | Ast.Toplevel expr ->
        begin
          let e = Ast.Function (Ast.Prototype ("", [||]), expr) in
          add_module Codegen.the_module the_execution_engine;
          (* Evaluate a top-level expression into an anonymous function. *)
          print_endline "parsed a top-level expr";
          anonymous_func_count := !anonymous_func_count + 1;
          let tmp_name = Format.sprintf "_anonymous_func_%d" !anonymous_func_count in
          let tmp_func = Ast.set_func_name tmp_name e in
          let the_function = Codegen.codegen_func the_fpm tmp_func in
          dump_value the_function;

          (* JIT the function, returning a function pointer. *)
          let fp = get_function_address
                     tmp_name (Foreign.funptr Ctypes.(void @-> returning double))
                     the_execution_engine in
          Printf.printf "Evaluated to %f\n" (fp ());
          remove_module Codegen.the_module the_execution_engine;
        end
     end;
     print_string "ready> "; flush stdout;
     main_loop the_fpm the_execution_engine
  end

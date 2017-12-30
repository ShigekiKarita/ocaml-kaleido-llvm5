(*===----------------------------------------------------------------------===
 * Top-Level parsing and JIT Driver
 *===----------------------------------------------------------------------===*)

open Batteries
module L = Llvm
module E = Llvm_executionengine


(* top ::= definition | external | expression | ';' *)
let main_loop the_fpm the_execution_engine input =
  let eval_count = ref 0 in
  let rec go count =
    let rec eval_ast result =
      Printf.printf "AST: %s\n" Ast.(show result);
      begin
        try
          match result with
          | Ast.Semicolon -> (); (* ignore top-level semicolons. *)
          | Ast.Definition e ->
             begin
               print_endline "parsed a function definition.";
               L.dump_value (Codegen.codegen_func the_fpm e);
             end
          | Ast.Extern e ->
             begin
               print_endline "parsed an extern.";
               L.dump_value (Codegen.codegen_proto e);
             end
          | Ast.Toplevel expr ->
             begin
               (* Evaluate a top-level expression into an anonymous function. *)
               print_endline "parsed a top-level expr";
               let tmp_name = Format.sprintf "_anonymous_func_%d" !eval_count in
               eval_count := !eval_count + 1;
               let tmp_func = Ast.Function (Ast.Prototype (tmp_name, [||]), expr) in

               (* JIT the function, returning a function pointer. *)
               E.add_module Codegen.the_module the_execution_engine;
               let the_function = Codegen.codegen_func the_fpm tmp_func in
               L.dump_value the_function;
               let fp = E.get_function_address
                          tmp_name (Foreign.funptr Ctypes.(void @-> returning double))
                          the_execution_engine in
               Printf.printf "Evaluated to %f\n" (fp ());
               E.remove_module Codegen.the_module the_execution_engine;
             end
        with Codegen.Error s ->
          (* Skip token for error recovery. *)
          Printf.eprintf "Error%s" s;
          prerr_newline ();
      end in
    Parser.entry_point Lexer.token input
    |> function
      | None -> ()
      | Some r ->
         begin
           eval_ast r;
           print_string "ready> ";
           flush stdout;
         end;
         go (count + 1)
  in go 0

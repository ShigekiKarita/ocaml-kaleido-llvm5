(*===----------------------------------------------------------------------===
 * Top-Level parsing and JIT Driver
 *===----------------------------------------------------------------------===*)

module L = Llvm
module E = Llvm_executionengine


(* top ::= definition | external | expression | ';' *)
let main_loop the_fpm the_execution_engine =
  (* TODO support multiple expressions, externals and definitions *)
  let rec go count =
    Parser.entry_point Lexer.token (Lexing.from_channel stdin)
    |> function
      | None -> ()
      | Some result ->
         Printf.printf "AST: %s\n" Ast.(show result);
         begin match result with
         | Ast.Semicolon -> () (* ignore top-level semicolons. *)
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
              let e = Ast.Function (Ast.Prototype ("", [||]), expr) in
              E.add_module Codegen.the_module the_execution_engine;
              (* Evaluate a top-level expression into an anonymous function. *)
              print_endline "parsed a top-level expr";
              let tmp_name = Format.sprintf "_anonymous_func_%d" count in
              let tmp_func = Ast.set_func_name tmp_name e in
              let the_function = Codegen.codegen_func the_fpm tmp_func in
              L.dump_value the_function;

              (* JIT the function, returning a function pointer. *)
              let fp = E.get_function_address
                         tmp_name (Foreign.funptr Ctypes.(void @-> returning double))
                         the_execution_engine in
              Printf.printf "Evaluated to %f\n" (fp ());
              E.remove_module Codegen.the_module the_execution_engine;
            end
         end;
         print_string "ready> ";
         flush stdout;
         go (count + 1)
  in go 0

(*===----------------------------------------------------------------------===
 * Top-Level parsing and JIT Driver
 *===----------------------------------------------------------------------===*)

open Llvm
open Llvm_executionengine

let anonymous_func_count = ref 0

(* top ::= definition | external | expression | ';' *)
let rec main_loop the_fpm the_execution_engine stream =
  match Stream.peek stream with
  | None -> ()

  (* ignore top-level semicolons. *)
  | Some (Token.Kwd ';') ->
      Stream.junk stream;
      main_loop the_fpm the_execution_engine stream

  | Some token ->
      begin
        try match token with
        | Token.Def ->
            let e = Parser.parse_definition stream in
            print_endline "parsed a function definition.";
            dump_value (Codegen.codegen_func the_fpm e);
        | Token.Extern ->
            let e = Parser.parse_extern stream in
            print_endline "parsed an extern.";
            dump_value (Codegen.codegen_proto e);
        | _ ->
           add_module Codegen.the_module the_execution_engine;

           (* Evaluate a top-level expression into an anonymous function. *)
           let e : Ast.func = Parser.parse_toplevel stream in
           print_endline "parsed a top-level expr";
           anonymous_func_count := !anonymous_func_count + 1;
           let tmp_name = Format.sprintf "_anonymous_func_%d" !anonymous_func_count in
           let tmp_func = Ast.set_func_name tmp_name e in
           let the_function = Codegen.codegen_func the_fpm tmp_func in
           dump_value the_function;

           (* JIT the function, returning a function pointer. *)
           let fp = get_function_address tmp_name (Foreign.funptr Ctypes.(void @-> returning double)) the_execution_engine in
           Printf.printf "Evaluated to %f\n" (fp ());

           remove_module Codegen.the_module the_execution_engine

        with Stream.Error s | Codegen.Error s ->
          (* Skip token for error recovery. *)
          Stream.junk stream;
          print_endline s;
      end;
      print_string "ready> "; flush stdout;
      main_loop the_fpm the_execution_engine stream

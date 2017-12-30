open Batteries

let to_ast s = Lexing.from_string s |> Parser.parse_toplevel Lexer.token

let to_ast_def s = Lexing.from_string s |> Parser.parse_definition Lexer.token

let to_ast_ext s = Lexing.from_string s |> Parser.parse_extern Lexer.token

let () =
  assert (to_ast "f(1,2)" = Ast.Call ("f", [|Ast.Number 2.; Ast.Number 1.|]));
  assert (to_ast "1+2" = Ast.Binary ("+", (Ast.Number 1.), (Ast.Number 2.)));
  let def = to_ast_def "def f(x) x + 1" in
  Format.printf "%s\n" Ast.(show_func def);
  let ext = to_ast_ext "extern sin(x)" in
  Format.printf "%s\n" Ast.(show_proto ext);


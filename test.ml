open Batteries

let to_ast_top s = Lexing.from_string s |> Parser.parse_toplevel Lexer.token

let to_ast_def s = Lexing.from_string s |> Parser.parse_definition Lexer.token

let to_ast_ext s = Lexing.from_string s |> Parser.parse_extern Lexer.token

let () =
  assert (to_ast_top "f(1,2)" = Ast.Call ("f", [|Ast.Number 2.; Ast.Number 1.|]));
  assert (to_ast_top "1+2" = Ast.Binary ("+", (Ast.Number 1.), (Ast.Number 2.)));
  let def = to_ast_def "def f(x) x + 1" in
  Format.printf "%s\n" Ast.(show_func def);
  let ext = to_ast_ext "extern sin(x)" in
  Format.printf "%s\n" Ast.(show_proto ext);
  let ifdef = to_ast_def "def baz(x) if x then foo() else bar()" in
  Format.printf "%s\n" Ast.(show_func ifdef);
  let forex = to_ast_top "for i=1, i<10, 1 in putchard(i)" in
  Format.printf "%s\n" Ast.(show_expr forex);



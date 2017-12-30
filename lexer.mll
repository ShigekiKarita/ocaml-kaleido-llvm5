{
  open Batteries
  module P = Parser
  exception Error of string
}

(* let int = '-'? ['0'-'9'] ['0'-'9']* *)
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let identifier = ['A'-'Z' 'a'-'z'] ['A'-'z' 'a'-'z' '0'-'9']*

rule token = parse
  (* TODO skip comment *)
  | [' ' '\r' '\t' '\n']    { token lexbuf }     (* skip blanks *)
  | "def"                   { P.DEF }
  | "extern"                { P.EXTERN }
  | identifier as ident     { P.IDENT ident }
  | float as lxm            { P.NUMBER (float_of_string lxm) }
    (* | '='                     { P.SET } *)
  | '<' as op               { P.LT (String.of_char op) }
  | '>' as op               { P.GT (String.of_char op) }
  | "<="                    { P.LE "<=" }
  | ">="                    { P.GE ">=" }
  | '+' as op               { P.PLUS (String.of_char op) }
  | '-' as op               { P.MINUS (String.of_char op) }
  | '*' as op               { P.TIMES (String.of_char op) }
  | '/' as op               { P.DIV (String.of_char op) }
  | '('                     { P.LPAREN }
  | ')'                     { P.RPAREN }
  | eof                     { P.EOF }
  | ';'                     { P.SEMICOLON }
  | ','                     { P.COMMA }
  | _
    {
      let tok = Lexing.lexeme lexbuf in
      let pos = Lexing.lexeme_start_p lexbuf in
      let pos_fmt = Format.sprintf "file: %s, line: %d, col: %d" pos.pos_fname pos.pos_lnum pos.pos_cnum in
      raise (Error (Format.sprintf "unknown token: '%s' at (%s)" tok pos_fmt))
    }

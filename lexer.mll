{
    module P = Parser
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
  | '+' as op               { P.PLUS op }
  | '-' as op               { P.MINUS op }
  | '*' as op               { P.TIMES op }
  | '/' as op               { P.DIV op }
  | '('                     { P.LPAREN }
  | ')'                     { P.RPAREN }
  | eof                     { P.EOF }
  | ';'                     { P.SEMICOLON }
  | ','                     { P.COMMA }
  | _ { raise (Failure (Format.sprintf "don't know how to handle '%s'" (Lexing.lexeme lexbuf))) }

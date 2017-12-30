%{
%}

%token <float> NUMBER
%token <string> IDENT
%token LPAREN RPAREN

%token KWD_DEF KWD_EXTERN
       KWD_IF KWD_THEN KWD_ELSE
       KWD_FOR KWD_IN

%token ASSIGN
%token <string> PLUS MINUS TIMES DIV LT GT LE GE

(* %token SET *)
(* %right SET *)
(* TODO define precedence *)
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
(* %nonassoc UMINUS        /* highest precedence */ *)
%token EOF
%token SEMICOLON
%token COMMA

%start parse_toplevel
%type <Ast.expr> parse_toplevel

%start parse_definition
%type <Ast.func> parse_definition

%start parse_extern
%type <Ast.proto> parse_extern

%start entry_point
%type <Ast.t option> entry_point


%%

(* Menhir let us give names to symbol values,
   instead of having to use $1, $2, $3 as in ocamlyacc *)

expr
  : n = NUMBER
    { Ast.Number n }
  | LPAREN e=expr RPAREN
    { e }
  | id=IDENT
    { Ast.Variable id }
  | id=IDENT LPAREN args=argument_expr RPAREN
    { Ast.Call (id, Array.of_list @@ List.rev args) }
  | id=IDENT LPAREN RPAREN
    { Ast.Call (id, [||]) }
  | e1=expr op=bin_op_expr e2=expr
    { Ast.Binary (op, e1, e2) }
  | KWD_IF c=expr KWD_THEN t=expr KWD_ELSE e=expr
    { Ast.If (c, t, e) }
  | KWD_FOR id=IDENT ASSIGN start_=expr COMMA end_=expr KWD_IN body=expr
    { Ast.For (id, start_, end_, None, body) }
  | KWD_FOR id=IDENT ASSIGN start_=expr COMMA end_=expr COMMA step=expr KWD_IN body=expr
    { Ast.For (id, start_, end_, Some step, body) }
  ;

argument_expr
  : head_arg=expr COMMA rest_args=argument_expr { head_arg :: rest_args }
  | last_arg=expr { [last_arg] }
  ;

bin_op_expr
  : PLUS | MINUS | TIMES | DIV | LT | GT | LE | GE { $1 }
  ;

definition
  : KWD_DEF p=prototype e=expr
    { Ast.Function (p, e) }
  ;

extern
  : KWD_EXTERN p=prototype { p }
  ;

argument_proto
  : head_arg=IDENT COMMA rest_args=argument_proto { head_arg :: rest_args }
  | last_arg=IDENT { [last_arg] }
  ;

prototype
  : id=IDENT LPAREN args=argument_proto RPAREN
    { Ast.Prototype (id, Array.of_list (List.rev args)) }
  | id=IDENT LPAREN RPAREN
    { Ast.Prototype (id, [||]) }
  ;


parse_toplevel: e = expr EOF { e };

parse_definition: d=definition EOF { d };

parse_extern: x=extern EOF { x }

abstract_syntax
  : e=expr { Ast.Toplevel e }
  | d=definition { Ast.Definition d }
  | x=extern { Ast.Extern x }
  ;

entry_point
  : ast=abstract_syntax SEMICOLON { Some ast }
  | SEMICOLON { Some Ast.Semicolon }
  | EOF { None }
  ;

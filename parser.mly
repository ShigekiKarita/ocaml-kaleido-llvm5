%{
%}

%token <float> NUMBER
%token <string> IDENT
%token DEF
%token EXTERN
%token <char> PLUS MINUS TIMES DIV
%token LPAREN RPAREN
(* %token SET *)
(* %right SET *)

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
%type <Ast.t list> entry_point


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
  ;

argument_expr
  : head_arg=expr COMMA rest_args=argument_expr { head_arg :: rest_args }
  | last_arg=expr { [last_arg] }
  ;

bin_op_expr
  : c=PLUS | c=MINUS | c=TIMES | c=DIV { c }
  ;

definition
  : DEF p=prototype e=expr
    { Ast.Function (p, e) }
  ;

extern
  : EXTERN p=prototype { p }
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
  : head=abstract_syntax SEMICOLON { [head; Ast.Semicolon] }
  | head=abstract_syntax SEMICOLON rest=entry_point { head :: Ast.Semicolon :: rest }
  | SEMICOLON { [Ast.Semicolon] }
  | EOF { [] }
  ;

%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token PLUS MINUS TIMES DIV NEG NOT EQ LESS GREATER OR AND
%token IF THEN ELSE TRUE FALSE UNIT
%token LPAREN RPAREN ANNOTATION ARROW LAMBDA SEMI DOT FIX LET SET IN
%token BOOLTYPE INTTYPE UNITTYPE TYPEARROW AFFINE ATMOST ATLEAST
%token EOF

%type <Ast.datatype> typexpr
%type <Ast.exp> expr
%type <Ast.modifier> modifier

(* we split into different kinds of expressions to define associativity
   this can't be done automatically with %left %right because of 
   the function application lol
*)
%type <Ast.exp> expr2
%type <Ast.exp> expr3
%type <Ast.exp> expr4
%type <Ast.exp> expr5
%type <Ast.exp> expr6
%type <Ast.exp> expr7
%type <Ast.exp> expr8

%right TYPEARROW ARROW

%left APPLY

%start <Ast.exp> main
%%

main:
  | expr EOF { $1 }

expr:
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | LET IDENT ANNOTATION typexpr SET expr IN expr { Let ($2, ($4, NoMod), $6, $8) }
  | LAMBDA IDENT ANNOTATION typexpr ARROW expr { Lam ($2, ($4, NoMod), $6) }
  | LET IDENT ANNOTATION typexpr SEMI modifier SET expr IN expr { Let ($2, ($4, $6), $8, $10) }
  | LAMBDA IDENT ANNOTATION typexpr SEMI modifier ARROW expr { Lam ($2, ($4, $6), $8) }
  | FIX expr { Fix $2 }
  | expr2 {$1}

expr2:
  | expr2 OR expr3 {Or ($1, $3)}
  | expr3 { $1 }

expr3:
  | expr3 AND expr4 {And ($1, $3)}
  | expr4 { $1 }

expr4:
  | expr4 EQ expr5 { Eq ($1, $3) }
  | expr4 LESS expr5 { Less ($1, $3) }
  | expr4 GREATER expr5 { More ($1, $3) }
  | expr5 {$1}

(* MUL & DIV: notice that by putting a lower precedence on the left side, 
   we essentially force it to be left associative *)
expr5:
  | expr5 PLUS expr6 { Plus ($1, $3) }
  | expr5 MINUS expr6 { Minus ($1, $3) }
  | NEG expr6 { Neg $2 }
  | NOT expr6 { Not $2 }
  | expr6 { $1 }

(* MUL & DIV: notice that by putting a lower precedence on the left side, 
   we essentially force it to be left associative *)
expr6:
  | expr6 TIMES expr7 { Times ($1, $3) }
  | expr6 DIV expr7 { Div ($1, $3) }
  | expr7 { $1 }

expr7:
  | expr7 expr8 %prec APPLY { App ($1, $2) }
  | expr8 {$1}

expr8:
  | INT { Nat $1 }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | UNIT { Unit }
  | IDENT { X $1 }
  | LPAREN expr RPAREN { $2 }

typexpr:
  | UNITTYPE { UnitType }
  | INTTYPE { IntType }
  | BOOLTYPE { BoolType }
  | typexpr TYPEARROW typexpr {FnType ($1, $3)}
  | LPAREN typexpr RPAREN { $2 }

modifier:
  | AFFINE {AtMost 1}
  | ATMOST INT {AtMost $2}
  | ATLEAST INT {AtLeast $2}
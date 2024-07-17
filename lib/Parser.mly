%{
  open Ast
%}

%token <int> INT
%token <string> IDENT
%token PLUS MINUS TIMES DIV NEG NOT EQ LESS GREATER
%token IF THEN ELSE TRUE FALSE UNIT
%token LPAREN RPAREN ANNOTATION ARROW LAMBDA SEMI DOT FIX LET SET IN
%token BOOLTYPE INTTYPE UNITTYPE TYPEARROW AFFINE ATMOST ATLEAST
%token EOF

%type <Ast.datatype> typexpr
%type <Ast.exp> expr
%type <Ast.modifier> modifier

%left APP

%start <Ast.exp> main
%%

main:
  | expr EOF { $1 }

expr:
  | INT { Nat $1 }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | UNIT { Unit }
  | IDENT { X $1 }
  | expr PLUS expr { Plus ($1, $3) }
  | expr MINUS expr { Minus ($1, $3) }
  | expr TIMES expr { Times ($1, $3) }
  | expr DIV expr { Div ($1, $3) }
  | NEG expr { Neg $2 }
  | NOT expr { Not $2 }
  | expr EQ expr { Eq ($1, $3) }
  | expr LESS expr { Less ($1, $3) }
  | expr GREATER expr { More ($1, $3) }
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | LET IDENT ANNOTATION typexpr SET expr IN expr { Let ($2, ($4, NoMod), $6, $8) }
  | LAMBDA IDENT ANNOTATION typexpr ARROW expr { Lam ($2, ($4, NoMod), $6) }
  (*| LET IDENT ANNOTATION typexpr SEMI modifier SET expr IN expr { Let ($2, ($4, $6), $8, $10) }
  | LAMBDA IDENT ANNOTATION typexpr SEMI modifier ARROW expr { Lam ($2, ($4, $6), $8) }*)
  | FIX expr { Fix $2 }
  | expr DOT expr %prec APP { App ($1, $3) }  
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
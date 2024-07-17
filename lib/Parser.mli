
(* The type of tokens. *)

type token = 
  | UNITTYPE
  | UNIT
  | TYPEARROW
  | TRUE
  | TIMES
  | THEN
  | SET
  | SEMI
  | RPAREN
  | PLUS
  | OR
  | NOT
  | NEG
  | MINUS
  | LPAREN
  | LET
  | LESS
  | LAMBDA
  | INTTYPE
  | INT of (int)
  | IN
  | IF
  | IDENT of (string)
  | GREATER
  | FIX
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DOT
  | DIV
  | BOOLTYPE
  | ATMOST
  | ATLEAST
  | ARROW
  | ANNOTATION
  | AND
  | AFFINE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.exp)

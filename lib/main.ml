let parse_with_error lexbuf =
  try
    Parser.main Lexer.read lexbuf
  with
  (*| Lexer.SyntaxError msg ->
      Printf.fprintf stderr "%s%!" msg; exit (-1)*)
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf); exit (-1)

open Ast
open affine_typed_lc

let () =
  let lexbuf = Lexing.from_channel stdin in
  let program = parse_with_error lexbuf in
  let () = Printf.printf "Parsed program: %s\n" (string_of_exp program) in
  print_string (string_of_value eval2 program ^ "\n")
let parse_with_error lexbuf = Parser.main Lexer.token lexbuf
  (*try
    Parser.main Lexer.read lexbuf
  with
  (*| Lexer.SyntaxError msg ->
      Printf.fprintf stderr "%s%!" msg; exit (-1)*)
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf); exit (-1)
  | _ -> Printf.fprintf stderr "Unknown error occurred\n"; exit (-1)*)

open Ast
open Affine_typed_lc

(*let k = (Parser.main Lexer.read (Lexing.from_string "fn hi : unit => 5"))*)

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "Usage: %s <input_file>\n" Sys.argv.(0);
    exit 1
  end;

  let input_file = Sys.argv.(1) in
  let in_channel = open_in input_file in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let program = parse_with_error lexbuf in
    let () = close_in in_channel;
    Printf.printf "Parsed program: %s\n" (string_of_exp program) in
    print_string (string_of_value (eval2 program) ^ "\n")
  with
  | e ->
    close_in in_channel;
    raise e

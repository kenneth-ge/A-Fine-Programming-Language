SHELL = powershell.exe

all: Lexer.ml Ast.cmo Affine_typed_lc.cmo Parser.cmo Lexer.cmo main.cmo
	ocamlc -o main.exe Ast.cmo Lexer.cmo Parser.cmo Affine_typed_lc.cmo main.cmo

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

Ast.cmo: Ast.ml
	ocamlc -c Ast.ml

Affine_typed_lc.cmo: Affine_typed_lc.ml
	ocamlc -c Affine_typed_lc.ml

Parser.cmo: Ast.cmo Parser.mly
	menhir Parser.mly
	ocamlc -c Parser.mli
	ocamlc -c Parser.ml

Lexer.cmo: Lexer.ml Ast.cmo Parser.cmo
	ocamlc -c Lexer.ml

main.cmo: Ast.cmo Affine_typed_lc.cmo main.ml
	ocamlc -c main.ml

clean: clean_exe clean_temp

clean_exe:
	rm main.exe, main.out

clean_temp:
	rm Ast.cmi, Ast.cmo, Lexer.cmi, Lexer.cmo, Lexer.ml, main.cmi, main.cmo, Parser.cmi, Parser.cmo, Parser.ml, Parser.mli, Affine_typed_lc.cmo, Affine_typed_lc.cmi
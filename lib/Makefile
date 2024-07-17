SHELL = powershell.exe

CP = copy /B
RM = del

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

copy:
	cp ../affine_typed_lc.ml ./Affine_typed_lc.ml

clean:
	$(RM) Ast.cmi Ast.cmo Lexer.cmi Lexer.cmo Lexer.ml main main.cmi main.cmo Parser.cmi Parser.cmo Parser.ml Parser.mli

clean_extra:
	$(RM) Ast.cmi Ast.cmo Lexer.cmi Lexer.cmo Lexer.ml main.cmi main.cmo Parser.cmi Parser.cmo Parser.ml Parser.mli
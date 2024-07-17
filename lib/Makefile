SHELL = powershell.exe

CP = copy /B
RM = del

all:
	ocamllex Lexer.mll
	ocamlc -c Ast.ml
	ocamlc -c Affine_typed_lc.ml
	menhir Parser.mly
	ocamlc -c Parser.mli
	ocamlc -c Parser.ml
	ocamlc -c Lexer.ml
	ocamlc -c main.ml
	ocamlc -o main.exe Ast.cmo Lexer.cmo Parser.cmo Affine_typed_lc.cmo main.cmo

copy:
	cp ../affine_typed_lc.ml ./Affine_typed_lc.ml

clean:
	$(RM) Ast.cmi Ast.cmo Lexer.cmi Lexer.cmo Lexer.ml main main.cmi main.cmo Parser.cmi Parser.cmo Parser.ml Parser.mli

clean_extra:
	$(RM) Ast.cmi Ast.cmo Lexer.cmi Lexer.cmo Lexer.ml main.cmi main.cmo Parser.cmi Parser.cmo Parser.ml Parser.mli
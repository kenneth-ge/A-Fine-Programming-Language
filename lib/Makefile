SHELL = powershell.exe

CP = copy /B
RM = del

all:
	$(CP) ../affine_typed_lc.ml .
	ocamlc -c affine_typed_lc.ml
	ocamllex Lexer.mll
	ocamlc -c Ast.ml
	menhir Parser.mly --infer
	ocamlc -c Parser.mli
	ocamlc -c Parser.ml
	ocamlc -c Lexer.ml
	ocamlc -c main.ml
	ocamlc -o main Ast.cmo Lexer.cmo Parser.cmo main.cmo affine_typed_lc.cmo

clean:
	$(RM) Ast.cmi Ast.cmo Lexer.cmi Lexer.cmo Lexer.ml main main.cmi main.cmo Parser.cmi Parser.cmo Parser.ml Parser.mli

clean_extra:
	$(RM) Ast.cmi Ast.cmo Lexer.cmi Lexer.cmo Lexer.ml main.cmi main.cmo Parser.cmi Parser.cmo Parser.ml Parser.mli
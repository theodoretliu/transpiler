transpiler: transpiler.ml
	ocamlbuild transpiler.native

clean:
	rm -rf _build *.native

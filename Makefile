.PHONY : all
all : drrty.native

drrty.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind drrty.native

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff
OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt -g

all: matcher.cma matcher.cmxa

matcher.cma:
	$(OCAMLC) -c matcher.mli
	$(OCAMLC) -c matcher.ml
	$(OCAMLC) -c matcher_stubs.c
	ocamlmklib -o matcher_stubs matcher_stubs.o
	$(OCAMLC) -a -o $@ matcher.cmo -dllib -lmatcher_stubs

matcher.cmxa:
	$(OCAMLOPT) -c matcher.mli
	$(OCAMLOPT) -c matcher.ml
	#$(OCAMLOPT) -c matcher_stubs.c
	gcc -O3 -g -ggdb -I /home/def/.opam/4.04.2/lib/ocaml -c matcher_stubs.c
	$(OCAMLOPT) -a -o $@ matcher.cmx matcher_stubs.o

test: matcher.cmxa
	ocamlopt -o test matcher.cmxa test.ml

.PHONY: matcher.cma matcher.cmxa test

# Makefile for parallel password cracker

CC=gcc
CFLAGS=-O2 -Wall -Wno-unused
CLIBS=
CINC=-I/usr/lib/ocaml

OCAMLC = ocamlc.opt
OCAMLOPT = ocamlopt.opt
OCAMLDEP = ocamldep
OCAMLNCFLAGS = -unsafe
OCAMLLIB = /usr/lib/ocaml
OCAMLFIND = /usr/bin/ocamlfind
OCAMLLIBS = 

LIBSRC = stemmer.ml
LIBSRC_C = stemmer.c
LIBOBJ = $(LIBSRC:.ml=.cmo)
LIBOBJ_X = $(LIBSRC:.ml=.cmx)
LIBOBJ_O = stemmer.o

all: stemmer.cmi stemmer.cma stemmer.cmxa

stemmer.a: $(LIBOBJ_O)
	ar cruvs $@ $^

stemmer.cma: $(LIBOBJ)
	$(OCAMLFIND) ocamlc -a -o $@ -package "$(OCAMLLIBS)" -linkpkg $(LIBOBJ) \
		-ccopt "$(CFLAGS) $(CLIBS)"

stemmerC.cma: stemmerC.cmo stemmer.a
	$(OCAMLFIND) ocamlc -a -o $@ -package "$(OCAMLLIBS)" -linkpkg stemmer.a \
		stemmerC.cmo -ccopt "$(CFLAGS) $(CLIBS)"

stemmer.cmxa: $(LIBOBJ_X) 
	$(OCAMLFIND) ocamlopt -a -o $@ -package "$(OCAMLLIBS)" -linkpkg $(LIBOBJ_X) \
		-ccopt "$(CFLAGS) $(CLIBS)"

install: all stemmer.cmi META
	$(OCAMLFIND) install stemmer stemmer.cma stemmer.cmxa stemmer.cmi stemmer.mli META

stcaml: stemmer.c stemmer.cma stemmerC.cma
	ocamlmktop -custom -ccopt -ggdb -o $@ $^

test: stcaml
	./stcaml test.ml

clean:
	rm -f $(PROG) *.o *.cmo *.cmi *.a *.cmx *.cma *.cmxa stcaml

%.o : %.c
	$(CC) $(CFLAGS) $(CINC) -o $@ -c $<

%.cmo : %.ml
	$(OCAMLFIND) ocamlc -package "$(OCAMLLIBS)" -c $<

%.cmi : %.mli
	$(OCAMLC) $(OCAMLINCLUDES) $(OCAMLFLAGS) $(OCAMLBCFLAGS) -c $<

%.cmx : %.ml
	$(OCAMLOPT) $(OCAMLINCLUDES) $(OCAMLFLAGS) $(OCAMLNCFLAGS) -c $<

%.cma :
	$(OCAMLC) $(OCAMLINCLUDES) -a -o $@ $(OCAMLBCLDFLAGS) \
		$(OCAMLLIBS) $^ 

%.cmxa :
	$(OCAMLOPT) $(OCAMLINCLUDES) -a -o $@ $(OCAMLNCLDFLAGS) $^

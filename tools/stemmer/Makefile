#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

LOCALPREFIX=../..

# User configuration
-include $(LOCALPREFIX)/config/Makefile

# Generic configuration
include $(LOCALPREFIX)/config/Makefile.gen

OBJECTS      = stemmer.cmo
OPTOBJECTS   = $(OBJECTS:.cmo=.cmx)

smart:
	$(MAKE) all
	if test -f $(OCAMLOPT); then $(MAKE) opt; else :; fi

all: $(OBJECTS)

opt: $(OPTOBJECTS)

install:
	$(CP) *.cmi *.mli $(CAMLLIBDIR)

.depend: *.ml 
	$(OCAMLDEP) *.ml > .depend

clean:
	rm -f *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa

include .depend



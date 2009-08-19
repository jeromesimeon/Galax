#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.8 2007/02/01 22:08:54 simeon Exp $ #

#
# C and C++ APIs makefile
#

start:
	$(MAKE) all

LOCALPREFIX=../..
include ../../config/Makefile
include ../../config/Makefile.gen

# Caml compilation flags

INCLUDES=

OCAMLFLAGS=
OPTOCAMLFLAGS=

# Caml object files

OBJECTS=

# Target C binaries and libraries

TARGETOBJS=unix2dos.cmo

TARGET=unix2dos$(EXE)

# Production rules

all: $(TARGET)

$(TARGET): $(TARGETOBJS)
	$(OCAMLC) -custom -linkall -o $@ $(ALL_CAML_LIBS) $(LIB) $(TARGETOBJS) $(OCAMLFLAGS)

# Cleaning

clean:
	rm -f *.cm[oixa]
	rm -f $(TARGET)
	rm -f *.[oa] *.so *~

# Installation

install:

# Dependencies

.depend: 
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml | sed -e 's?\([a-z]\)\\?\1\/?g' > .depend

include .depend


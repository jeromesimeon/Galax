#(******************************************************)
#(*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   *)
#(*                   14/05/2000                       *)
#(******************************************************)

# [22-Aug-2002] Modified by Jerome Simeon
# [14-Jun-2001] Modified by Gerd Stolpmann

#########################################################################
# Author:   Christopher A. Rath (AT&T Labs Research)
# Descripton:
#     This is a replacement Makefile for the Galax C API.
# History:
#  $Log: Makefile,v $
#  Revision 1.6  2006/04/17 18:10:41  car
#  April 17, 2006 - Chris Rath
#    o Changes necessary to align with GODI
#
#########################################################################

#########################################################################
# Section:  Default target
# Description:
#     The default target for this Makefile is "all"
#########################################################################

default: all

#########################################################################
# Section:  Makefile pre-includes
# Description:
#     This is where the file(s) generated during by the Configure script
#     are included.  If config/Makefile.conf does not exist, the
#     make will fail.
#
#     Makefile.galax:   Variables defined for compiling and linking Galax
#                       applications in the build environment
#########################################################################
LOCALPREFIX=../..

include $(LOCALPREFIX)/config/Makefile.galax

#########################################################################
# Section:  Local variables
# Description:
#     These variables are used for targets of this Makefile
###########################################################################

TARGET=ucs2_to_utf8$(EXE)
OPTTARGET=ucs2_to_utf8$(OPT)

ALL_TARGETS=$(TARGET)

ifdef OCAMLOPT
ALL_TARGETS+=$(OPTTARGET)
endif

DEPOBJS = ucs2_to_utf8.ml lexer.ml parser.ml parser.mli types.ml

UCS2_TO_UTF8OBJS = types.cmo lexer.cmo parser.cmo ucs2_to_utf8.cmo
UCS2_TO_UTF8OPTOBJS = types.cmx lexer.cmx parser.cmx ucs2_to_utf8.cmx

#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the ucs2_to_utf8 programs
#  install::   This target does installation for ucs2_to_utf8 programs
#  uninstall:: This target uninstalls ucs2_to_utf8 programs
#  clean::     This target cleans files created in the all section
#########################################################################

all: $(ALL_TARGETS)

install::

uninstall::

clean::
	$(RM) *.cm[oixa] *.[oa] *~
	$(RM) $(CLEAN_LIST) lexer.ml parser.ml parser.mli
	$(RM) $(ALL_TARGETS)

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

lexer.ml:
	$(OCAMLLEX) lexer.mll

parser.ml: parser.mly
	$(OCAMLYACC) parser.mly

parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

$(TARGET): $(UCS2_TO_UTF8OBJS)
	$(OCAMLC) -o $(TARGET) $(UCS2_TO_UTF8OBJS)

$(OPTTARGET): $(UCS2_TO_UTF8OPTOBJS)
	$(OCAMLOPT) -o $(OPTTARGET) $(UCS2_TO_UTF8OPTOBJS)

#########################################################################
# Section:  Makefile post-includes
# Description:
#     This is where the dependency file is included and generated
#     We place the dependencies to make sure that various files
#     are generated before we need them.
#########################################################################
updatedepend::
	$(MAKE) .depend DEPDEPEND=1

clobber::
	$(RM) .depend

ifdef DEPDEPEND
.depend:	$(DEPOBJS)
	$(OCAMLDEP) $(DEPOBJS) | sed -e 's?\([a-z]\)\\?\1\/?g' > .depend
else
.depend:
	$(OCAMLDEP) $(DEPOBJS) | sed -e 's?\([a-z]\)\\?\1\/?g' > .depend
endif

ifndef NODEPEND
include .depend
endif


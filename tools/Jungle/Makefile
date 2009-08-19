#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.19 2007/02/01 22:08:54 simeon Exp $ #

#########################################################################
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax Jungle DM library
# History:
#	$Log: Makefile,v $
#	Revision 1.19  2007/02/01 22:08:54  simeon
#	February 1, 2007 - Jerome
#	
#	  o Code cleanup:
#	     - Cleaned up all the source file headers. Added module
#	       descriptions when missing, as well as CVS Id.
#	     - Removed obsolete modules: Optimization_rules_treepattern_old,
#	       Factorize_sideeffects.
#	
#	  o AST Walker:
#	     - Added support for a generic fold operation on the AST (useful
#	       to compute a boolean property on the AST for instance).
#	
#	  o Normalization:
#	     - Small fix to the normalization of comparisons to re-enable join
#	       detection, not using let bindings for the comparator
#	       anymore. [hack]
#	
#	  o Rewriting:
#	     - Added a judgment to check for side-effects, removed
#	       corresponding obsolete judgment in Factorization.
#	     - Moved the snap removal rule from optimization to rewriting,
#	       cleaning up the plans as early as possible.
#	
#	Revision 1.18  2006/05/15 15:20:37  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.17  2006/05/12 18:15:01  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.16  2006/04/17 18:10:40  car
#	April 17, 2006 - Chris Rath
#	  o Changes necessary to align with GODI
#	
#########################################################################

#########################################################################
# Section:	Default target
# Description:
#		The default target for this Makefile is "all"
#########################################################################

default:	all

#########################################################################
# Section:	Makefile pre-includes
# Description:
#		This is where the file(s) generated during by the Configure script
#		are included.  If config/Makefile.conf does not exist, the
#		make will fail.
#
#		Makefile.galax:	Variables defined for compiling and linking Galax
#								applications in the build environment
#########################################################################
LOCALPREFIX=../..

include $(LOCALPREFIX)/config/Makefile.galax

#########################################################################
# Section:	Local variables
# Description:
#		These variables are used for targets of this Makefile
###########################################################################

JUNGLE_INCLUDES=-I . -I ./shredded_jungle -I $(CONF_C_LIBBDB_INC)
JUNGLE_STUB_CLIBS=-L$(CONF_C_LIBBDB) -l$(CONF_C_LIBBDB_NAME) -L$(CONF_OCAML_IDL) -lcamlidl -lpthread

JUNGLE_IDLFILES=\
smalldb.ml \
smalldb.mli \
smalldb_stubs.c 

JUNGLE_CAML_OBJECTS=\
jungle.cmo \
smalldb.cmo \
shredded_jungle/shredded_jungle_record.cmo \
shredded_jungle/shredded_jungle_basetypes.cmo \
shredded_jungle/shredded_jungle_recno.cmo \
shredded_jungle/shredded_jungle_btree.cmo \
shredded_jungle/shredded_jungle_hash.cmo \
shredded_jungle/shredded_jungle_store.cmo \
shredded_jungle/shredded_jungle_dm.cmo

JUNGLE_CAML_OPTOBJECTS=$(JUNGLE_CAML_OBJECTS:.cmo=.cmx)

JUNGLE_CAML_MLFILES=$(JUNGLE_CAML_OBJECTS:.cmo=.ml)
JUNGLE_CAML_MLIFILES=\
smalldb.mli \
jungle.mli \
shredded_jungle/shredded_jungle_record.mli

JUNGLE_CAML_CMIFILES=\
$(JUNGLE_CAML_MLIFILES:.mli=.cmi) \
shredded_jungle/shredded_jungle_basetypes.cmi \
shredded_jungle/shredded_jungle_btree.cmi \
shredded_jungle/shredded_jungle_dm.cmi \
shredded_jungle/shredded_jungle_hash.cmi \
shredded_jungle/shredded_jungle_recno.cmi \
shredded_jungle/shredded_jungle_store.cmi

JUNGLE_C_OBJECTS=\
smallapi.o \
smalldb_stubs.o

JUNGLE_LIB=jungle.cma
JUNGLE_OPTLIB=$(JUNGLE_LIB:.cma=.cmxa)
JUNGLE_CLIB=$(JUNGLE_LIB:.cma=.a)
JUNGLE_STUB_CLIB=lib$(JUNGLE_LIB:.cma=.a)

JUNGLE_LIB_TARGETS=$(JUNGLE_LIB) $(JUNGLE_STUB_CLIB)

ifdef OCAMLOPT
JUNGLE_LIB_TARGETS+=$(JUNGLE_OPTLIB)
JUNGLE_CLIB_TARGETS+=$(JUNGLE_CLIB)
endif

JTARGETNAME2=shredded-jungle-load
JTARGET2=$(JTARGETNAME2)$(EXE)
JBYTETARGET2=$(JTARGETNAME2)$(BYTE)
JOPTTARGET2=$(JTARGETNAME2)$(OPT)
JTARGETOBJS2=$(JTARGETNAME2).cmo

JUNGLE_ALL_LNCOMMANDS=\
$(JTARGET2)

JUNGLE_ALL_COMMANDS=\
$(JBYTETARGET2)

JUNGLE_ALL_COMMAND_OBJS=\
$(JTARGETOBJS2)

ifdef OCAMLOPT
JUNGLE_ALL_COMMANDS+=\
$(JOPTTARGET2)

JUNGLE_ALL_COMMAND_OBJS+=\
$(JTARGETOBJS2:.cmo=.cmx)
endif

#########################################################################
# Sub-Section:	C compilation variables and rules
# Description:
#		These are the variables and rules for building C object files
###########################################################################
CFLAGS=-I$(CONF_OCAML_STDLIB) $(JUNGLE_INCLUDES)

#########################################################################
# Sub-Section:	OCaml compilation variables and rules
# Description:
#		These are the variables and rules for building C object files
###########################################################################
GALAX_ALL_INCLUDES+=-I shredded_jungle

#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the GALAX_CAPI_TARGETS
#  install::   This target does installation for C API files
#  uninstall:: This target uninstalls C API files
#  clean::     This target cleans files created in the all section
#########################################################################

all:	lib commands

byte:	bytelib bytecommands

install::

uninstall::

clean::

clobber::

#########################################################################
# Sub-Section:  lib targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

lib:	$(JUNGLE_LIB_TARGETS)

bytelib:	$(JUNGLE_LIB)

install::	lib $(CONF_GALAX_OCAMLLIB)
	for i in $(JUNGLE_LIB_TARGETS); do ($(CP) $$i $(CONF_GALAX_OCAMLLIB)); done
	if test -f $(CONF_GALAX_OCAMLLIB)/$(JUNGLE_STUB_CLIB); then $(RANLIB) $(CONF_GALAX_OCAMLLIB)/$(JUNGLE_STUB_CLIB) ; else :; fi
	if test -f $(JUNGLE_CLIB); then ($(CP) $(JUNGLE_CLIB) $(CONF_GALAX_OCAMLLIB); $(RANLIB) $(CONF_GALAX_OCAMLLIB)/$(JUNGLE_CLIB)); fi

uninstall::
	for i in $(JUNGLE_LIB_TARGETS); do ($(RM) $(CONF_GALAX_OCAMLLIB)/$$i); done
	$(RM) $(CONF_GALAX_OCAMLLIB)/$(JUNGLE_CLIB)

clean::
	$(RM) $(JUNGLE_LIB_TARGETS) $(JUNGLE_CLIB)

#########################################################################
# Sub-Sub-Section:  lib sub-targets
# Description:
#     These are the supporting targets for "lib"
#########################################################################

$(JUNGLE_LIB):	 $(JUNGLE_CAML_OBJECTS) smalldb.cmo
	$(OCAMLC) -a -linkall -o $(JUNGLE_LIB) $(JUNGLE_CAML_OBJECTS) $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_JUNGLE_LIBFLAGS)

$(JUNGLE_OPTLIB):	$(JUNGLE_CAML_OPTOBJECTS) smalldb.cmx
	$(OCAMLOPT) -a -linkall -o $(JUNGLE_OPTLIB) $(JUNGLE_CAML_OPTOBJECTS) $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_JUNGLE_LIBFLAGS)

$(JUNGLE_STUB_CLIB):	$(JUNGLE_C_OBJECTS)
	$(AR) rc $(JUNGLE_STUB_CLIB) $(JUNGLE_C_OBJECTS)
	if test $(RANLIB); then $(RANLIB) $(JUNGLE_STUB_CLIB); else :; fi

smalldb.ml: smalldb.idl
	$(CAMLIDL) smalldb.idl

smalldb.mli smalldb_stubs.c:	smalldb.ml

smalldb_stubs.o:	smalldb_stubs.c
	$(CC) $(CFLAGS) $(CCFLAGS) -c smalldb_stubs.c

install::	$(JUNGLE_CAML_MLIFILES) $(JUNGLE_CAML_CMIFILES)
	for i in $(JUNGLE_CAML_MLIFILES) $(JUNGLE_CAML_CMIFILES) ; do ($(CP) $$i $(CONF_GALAX_OCAMLLIB)); done

uninstall::
	for i in $(notdir $(JUNGLE_CAML_MLIFILES)); do ($(RM) $(CONF_GALAX_OCAMLLIB)/$$i); done

clean::
	$(RM) $(JUNGLE_IDLFILES)
	$(RM) *.o *.cmi
	$(RM) shredded_jungle/*.o shredded_jungle/*.cmi
	$(RM) $(JUNGLE_CAML_OBJECTS) smalldb.cmo
	$(RM) $(JUNGLE_CAML_OPTOBJECTS) smalldb.cmx
	$(RM) $(JUNGLE_CAML_MLIFILES:.mli=.cmi) smalldb.cmi

#########################################################################
# Sub-Section:  command targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

commands:	$(JUNGLE_ALL_COMMANDS)

bytecommands: $(JBYTETARGET2)

install:: commands $(CONF_GALAX_BIN)
	for i in $(JUNGLE_ALL_COMMANDS); do ($(CP) $$i $(CONF_GALAX_BIN)); done
	for i in $(JUNGLE_ALL_LNCOMMANDS); do ($(RM) $(CONF_GALAX_BIN)/$$i$(EXE)); (if test -f $(CONF_GALAX_BIN)/$$i$(OPT); then $(LN) $(CONF_GALAX_BIN)/$$i$(OPT) $(CONF_GALAX_BIN)/$$i$(EXE); else $(LN) $(CONF_GALAX_BIN)/$$i$(BYTE) $(CONF_GALAX_BIN)/$$i$(EXE); fi); done

uninstall::
	for i in $(JUNGLE_ALL_COMMANDS); do ($(RM) $(CONF_GALAX_BIN)/$$i); done
	for i in $(JUNGLE_ALL_LNCOMMANDS); do ($(RM) $(CONF_GALAX_BIN)/$$i); done

clean::
	$(RM) $(JUNGLE_ALL_COMMANDS) $(JUNGLE_ALL_COMMAND_OBJS)

$(JBYTETARGET2):	$(JUNGLE_LIB) $(JTARGETOBJS2)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(JTARGETOBJS2)

$(JOPTTARGET2):	$(JUNGLE_OPTLIB) $(JTARGETOBJS2:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(JTARGETOBJS2:.cmo=.cmx)

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
.depend: $(JUNGLE_CAML_MLFILES) $(JUNGLE_CAML_MLIFILES) $(JUNGLE_COMMAND_ALL_OBJS:.cmo=.ml)
	$(OCAMLDEP) $(GALAX_ALL_INCLUDES) $(JUNGLE_CAML_MLFILES) $(JUNGLE_CAML_MLIFILES) $(JUNGLE_COMMAND_ALL_OBJS:.cmo=.ml) | $(SED) -e 's?\([a-z]\)\\?\1\/?g' > .depend
else
.depend:
	$(OCAMLDEP) $(GALAX_ALL_INCLUDES) $(JUNGLE_CAML_MLFILES) $(JUNGLE_CAML_MLIFILES) $(JUNGLE_COMMAND_ALL_OBJS:.cmo=.ml) | $(SED) -e 's?\([a-z]\)\\?\1\/?g' > .depend
endif

ifndef NODEPEND
include .depend
endif


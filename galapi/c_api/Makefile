#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.34 2007/02/01 22:08:46 simeon Exp $ #

#########################################################################
# Author:   Christopher A. Rath (AT&T Labs Research)
# Descripton:
#     This is a replacement Makefile for the Galax C API.
# History:
#  $Log: Makefile,v $
#  Revision 1.34  2007/02/01 22:08:46  simeon
#  February 1, 2007 - Jerome
#
#    o Code cleanup:
#       - Cleaned up all the source file headers. Added module
#         descriptions when missing, as well as CVS Id.
#       - Removed obsolete modules: Optimization_rules_treepattern_old,
#         Factorize_sideeffects.
#
#    o AST Walker:
#       - Added support for a generic fold operation on the AST (useful
#         to compute a boolean property on the AST for instance).
#
#    o Normalization:
#       - Small fix to the normalization of comparisons to re-enable join
#         detection, not using let bindings for the comparator
#         anymore. [hack]
#
#    o Rewriting:
#       - Added a judgment to check for side-effects, removed
#         corresponding obsolete judgment in Factorization.
#       - Moved the snap removal rule from optimization to rewriting,
#         cleaning up the plans as early as possible.
#
#  Revision 1.33  2006/05/15 15:20:36  car
#  May 15, 2006 - Chris Rath
#    o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#      - Only creates the byte-code galax library and byte-code toplevel applications.
#    o Added a new target "byteworld" that works like "make world"
#  	 - Only creates the byte-code galax library and byte-code toplevel applications.
#
#  Revision 1.32  2006/04/17 20:56:36  mff
#
#  Added  $(OCAML_CAPI_OPTLIB_PATHS) to C-API linking step for
#  shared library libgalaxopt.so.
#
#  Revision 1.31  2006/04/17 18:10:40  car
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

#########################################################################
# Sub-Section:	OCaml Objects
# Description:
#		These are the OCaml object files that go into the C API library
###########################################################################
GALAX_CAPI_OBJECTS=galax_wrap.cmo
GALAX_CAPI_OPTOBJECTS=$(GALAX_CAPI_OBJECTS:.cmo=.cmx)
GALAX_CAPI_MLFILES=galax_wrap.ml
GALAX_CAPI_MLIFILES=galax_wrap.mli

#########################################################################
# Sub-Section:	C Objects
# Description:
#		These are the C object files that go into the C API library
###########################################################################
GALAX_CAPI_OPTCOBJECTS=galax_wrap_opt.o galax_util.o galax_stub.o itemlist.o

#########################################################################
# Sub-Section:	C Headers
# Description:
#		These are the C header files that are used by the C API library
###########################################################################
GALAX_CAPI_HEADERS=galax.h galax_util.h galax_types.h itemlist.h

#########################################################################
# Sub-Section:	C compilation variables and rules
# Description:
#		These are the variables and rules for building C object files
###########################################################################
CFLAGS+=$(CCFLAGS) $(GALAX_CAPI_INCLUDES)

.c.o: $(GALAX_CAPI_HEADERS)

#########################################################################
# Sub-Section:	Targets
# Description:
#		These are the configuration-dependent targets built by "all"
###########################################################################
GALAX_CAPI_TARGETS=$(GALAX_CAPI_OPTLIB)

ifeq ($(SUPPORTS_SHARED_LIBRARIES), true)
GALAX_CAPI_TARGETS+=$(GALAX_CAPI_OPTSHAREDLIB)
endif

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

all: $(GALAX_CAPI_TARGETS)

byte:	

install:: all $(CONF_GALAX_C_API)
	for i in $(GALAX_CAPI_TARGETS) $(GALAX_CAPI_HEADERS); do ($(CP) $$i $(CONF_GALAX_C_API)); done
	if test -f $(CONF_GALAX_C_API)/$(GALAX_CAPI_OPTLIB); then ($(RANLIB) $(CONF_GALAX_C_API)/$(GALAX_CAPI_OPTLIB)); fi

install::	$(CONF_GALAX_OCAMLLIB)
	for i in $(GALAX_CAPI_MLIFLES); do ($(CP) $$i $(CONF_GALAX_OCAMLLIB)); done

uninstall:
	for i in $(GALAX_CAPI_TARGETS) $(GALAX_CAPI_HEADERS); do ($(RM) $(CONF_GALAX_C_API)/$$i); done
	for i in $(GALAX_CAPI_MLIFLES); do ($(RM) $(CONF_GALAX_OCAMLLIB)/$$i); done

clean:
	rm -f *.cm[oixa]
	rm -f *.[oa] *.so *.dll *.dylib *.jnilib *~
	rm -f $(GALAX_CAPI_TARGETS)
	rm -f galax_wrap_byte.c

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

galax_wrap_opt.o: $(GALAX_OPTLIB_PATH) $(GALAX_CAPI_OPTOBJECTS)
	$(OCAMLOPT) -linkall -output-obj -o galax_wrap_opt.o -I $(LOCALPREFIX) $(GALAX_CAPI_INCLUDES) $(GALAX_ALL_OPTLIBS) $(GALAX_CAPI_OPTOBJECTS) $(GALAX_JUNGLE_LIBFLAGS)

$(GALAX_CAPI_OPTLIB): $(GALAX_CAPI_OPTCOBJECTS)
	$(AR) rc $(GALAX_CAPI_OPTLIB) $(GALAX_CAPI_OPTCOBJECTS)
	if test $(RANLIB); then $(RANLIB) $(GALAX_CAPI_OPTLIB); else :; fi

$(GALAX_CAPI_OPTSHAREDLIB): $(GALAX_CAPI_OPTCOBJECTS)
	$(MKSHAREDLIB) $(MKSHAREDLIBFLAGS) $(LDFLAGS) -o $(GALAX_CAPI_OPTSHAREDLIB) $(GALAX_CAPI_OPTCOBJECTS) $(OCAML_CAPI_OPTLIB_PATHS) $(OCAML_CAPI_OPTLIBS) $(GALAX_CAPI_SHAREDLIBFLAGS)

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
.depend: $(GALAX_CAPI_MLFILES) $(GALAX_CAPI_MLIFILES)
	$(OCAMLDEP) $(GALAX_INCLUDES) $(GALAX_CAPI_MLFILES) $(GALAX_CAPI_MLIFILES) | sed -e 's?\([a-z]\)\\?\1\/?g' > .depend
else
.depend:
	$(OCAMLDEP) $(GALAX_INCLUDES) $(GALAX_CAPI_MLFILES) $(GALAX_CAPI_MLIFILES) | sed -e 's?\([a-z]\)\\?\1\/?g' > .depend
endif

ifndef NODEPEND
include .depend
endif


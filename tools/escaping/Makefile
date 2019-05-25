#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.11 2007/02/01 22:08:54 simeon Exp $ #

#########################################################################
# Author:   Christopher A. Rath (AT&T Labs Research)
# Descripton:
#     This is a replacement Makefile for Galax that relies on the output
#     of a configuration script.  It is an attempt to clean up the
#     existing build process and make it more amenable to packaging
#     systems like GODI.
# History:
#  $Log: Makefile,v $
#  Revision 1.11  2007/02/01 22:08:54  simeon
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
#  Revision 1.10  2006/04/17 18:10:40  car
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
# Section:  Variables
# Description:
#     This is where variables are initialized that are used throughout
#     the Makefile and are not configuration-dependent.
#########################################################################

TARGETOBJS=escaping.cmo
SRCFILES=$(TARGETOBJECTS:.cmo=.ml)

TARGET=escaping$(EXE)

#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the bytecode versions of Galax
#  install::   This target does installation for files in the main section
#  uninstall:: This target uninstalls files installed in the main section
#  clean::     This target cleans files created in the all section
#
#########################################################################

all: $(TARGET)

install::

uninstall::

clean::
	rm -f *.cm[oixa]
	rm -f $(TARGET)
	rm -f *.[oa] *.so *~

#########################################################################
# Sub-Section: Secondary targets
# Description:
#     These are the targets listed as dependencies of "all".
#########################################################################

$(TARGET): $(TARGETOBJS)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(TARGETOBJS)

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
.depend: $(SRCFILES)
	$(OCAMLDEP) $(GALAX_INCLUDES) $(SRCFILES) | $(SED) -e 's?\([a-z]\)\\?\1\/?g' > .depend
else
.depend:
	$(OCAMLDEP) $(GALAX_INCLUDES) $(SRCFILES) | $(SED) -e 's?\([a-z]\)\\?\1\/?g' > .depend
endif

ifndef NODEPEND
include .depend
endif

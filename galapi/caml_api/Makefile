#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.28 2007/02/01 22:08:47 simeon Exp $ #

#########################################################################
# Author:   Christopher A. Rath (AT&T Labs Research)
# Descripton:
#     This is a replacement Makefile for the Galax OCaml API.
# History:
#  $Log: Makefile,v $
#  Revision 1.28  2007/02/01 22:08:47  simeon
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
#  Revision 1.27  2006/05/15 15:20:36  car
#  May 15, 2006 - Chris Rath
#    o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#      - Only creates the byte-code galax library and byte-code toplevel applications.
#    o Added a new target "byteworld" that works like "make world"
#  	 - Only creates the byte-code galax library and byte-code toplevel applications.
#
#  Revision 1.26  2006/05/12 18:15:00  car
#  May 12, 2006 - Chris Rath
#    o Fixed missing files and typos in Makefile.galax
#    o Added "regression" target to top level Makefile.
#    o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#    o Updated all lower-level Makefiles to ensure they all have the standard targets
#
#  Revision 1.25  2006/04/17 18:10:40  car
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
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the GALAX_CAPI_TARGETS
#  install::   This target does installation for C API files
#  uninstall:: This target uninstalls C API files
#  clean::     This target cleans files created in the all section
#########################################################################

all:

byte:

install: 	$(CONF_GALAX_OCAMLLIB)
	for i in $(GALAX_MLIFILES); do ($(CP) $$i $(CONF_GALAX_OCAMLLIB)); done
	for i in $(GALAX_CMIFILES); do (if test -f $$i; then $(CP) $$i $(CONF_GALAX_OCAMLLIB); fi); done

uninstall:
	for i in $(notdir $(GALAX_MLIFILES)); do ($(RM) $(CONF_GALAX_OCAMLLIB)/$$i); done
	for i in $(notdir $(GALAX_CMIFILES)); do ($(RM) $(CONF_GALAX_OCAMLLIB)/$$i); done

clean:

clobber::

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes
#########################################################################


#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.7 2007/02/01 22:08:54 simeon Exp $ #

#########################################################################
# Author:   Christopher A. Rath (AT&T Labs Research)
# Descripton:
#     This is a replacement Makefile for Galax that relies on the output
#     of a configuration script.  It is an attempt to clean up the
#     existing build process and make it more amenable to packaging
#     systems like GODI.
#
#		Note: This Makefile is obsolete.  The http.cmo and http.cmx files
#		are created in the main Makefile now.
# History:
#  $Log: Makefile,v $
#  Revision 1.7  2007/02/01 22:08:54  simeon
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
#  Revision 1.6  2006/04/17 18:10:40  car
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

OBJECTS      = http.cmo
OPTOBJECTS   = $(OBJECTS:.cmo=.cmx)

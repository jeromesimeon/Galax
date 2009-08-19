#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.2 2007/02/01 22:08:46 simeon Exp $ #

#########################################################################
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax example documents
# History:
#	$Log: Makefile,v $
#	Revision 1.2  2007/02/01 22:08:46  simeon
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
#	Revision 1.1  2006/05/17 15:15:05  car
#	May 17, 2006 Chris Rath
#	 o Added Makefile for examples/docs
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
_DOCFILES=\
hispo.xml \
hispo.xsd \
htmlpath.xq \
report.xml \
seq.xq \
seq_context.xq \
to_html.xml \
xml-example.xml \
xmlschema_context.xq \
xq-example1.xq \
xq-example2.xq

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

install: $(CONF_GALAX_EXAMPLES)/docs
	for i in $(_DOCFILES); do ($(CP) $$i $(CONF_GALAX_EXAMPLES)/docs); done

uninstall:
	for i in $(_DOCFILES); do ($(RM) $(CONF_GALAX_EXAMPLES)/docs/$$i); done

clean:

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

$(CONF_GALAX_EXAMPLES)/docs:
	$(MKDIR) $(CONF_GALAX_EXAMPLES)/docs

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


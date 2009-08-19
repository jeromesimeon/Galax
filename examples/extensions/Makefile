#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.35 2007/05/16 15:32:10 mff Exp $ #

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
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the TARGETS (temporarily disabled)
#  install::   This target does installation for OCaml API example files
#  uninstall:: This target uninstalls OCaml API example files
#  clean::     This target cleans files created in the all section
#########################################################################

all:

byte:

tests:

install: $(CONF_GALAX_EXAMPLES)/extensions
	$(CP) README $(CONF_GALAX_EXAMPLES)/extensions
	$(CP) xquery_bang.xq $(CONF_GALAX_EXAMPLES)/extensions
	$(CP) xquery_p.xq $(CONF_GALAX_EXAMPLES)/extensions
	$(CP) xquery_updates.xq $(CONF_GALAX_EXAMPLES)/extensions

uninstall:
	$(RM) $(CONF_GALAX_EXAMPLES)/extensions/README
	$(RM) $(CONF_GALAX_EXAMPLES)/extensions/xquery_bang.xq
	$(RM) $(CONF_GALAX_EXAMPLES)/extensions/xquery_p.xq
	$(RM) $(CONF_GALAX_EXAMPLES)/extensions/xquery_updates.xq

clean:
	$(RM) $(TARGETS)
	$(RM) *.cmx *.cmo *.o

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

$(CONF_GALAX_EXAMPLES)/extensions:
	$(MKDIR) $(CONF_GALAX_EXAMPLES)/extensions

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currenlty no post-includes for this Makefile
#########################################################################


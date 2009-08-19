#########################################################################
#                                                                       #
#                                  GALAX                                #
#                             XML Query Engine                          #
#                             Regression Tests                          #
#                                                                       #
#########################################################################

#########################################################################
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax documentation
# History:
#	$Log: Makefile,v $
#	Revision 1.21  2008/03/13 17:59:40  mff
#	Re-made doc for release
#	
#	Revision 1.20  2008/03/12 22:30:58  simeon
#	March 12, 2008 - Jerome
#	
#	  o Installation:
#	     - Now installing the extensions examples directory.
#	     - Added missing META.in.
#	
#	  o Serialization:
#	     - Fixed a few issues with serialization (showing up while doing
#	       the tutorial).
#	
#	  o Documentation:
#	     - Added table of contents back.
#	     - Pass on installation chapter. Added section on how to run the
#	       W3C XQuery test suite.
#	     - Removed the 'accessing and storing XML' part relating to Jungle
#	       and datamodel, empty or obsolete.
#	     - Added back chapter on APIs.
#	     - Shuffled the chapters in the reference part (alignment is
#	       renamed conformance and comes first, 'general' last).
#	     - Removed tutorial part on serialization as well-formed,
#	       buggy. Added text for more useful 'standard' mode.
#	     - Pass on tutorial chapter added a short tutorial section on
#	       printing compilation phases.
#	
#	  o Testing:
#	     - Fixed the default config file to not point to the
#	       Galax-specific test suite. (for distribution).
#	
#	Revision 1.19  2008/02/21 21:41:21  simeon
#	February 21, 2008 - Jerome
#	
#	  o Debian:
#	     - Folded in Stefano's patches.
#	
#	  o Documentation:
#	     - Re-enabled documentation generation.
#	     - Fixed some formatting issues.
#	
#	Revision 1.18  2007/10/25 00:08:41  mff
#	***** Tagged with galax-0-9-9 for Debian Release to Stefano Zacchiroli <zack@bononia.it> *****
#	
#	October 24, 2007 - Mary
#	
#	  o Documentation:
#	
#	    Woefully incomplete vis a vis Updates, XQueryP, APIs, etc.
#	
#	  o Code_selection:
#	
#	    Fixed insidious bug in code selection for references to imported
#	    variable or imported function: Incorrectly returned
#	    code-selection-context of imported module instead of importing
#	    module.
#	
#	  o configure script
#	
#	    Disabled C and Java APIs until we figure out what we want to do
#	    with them.
#	
#	  o OCaml API examples
#	
#	    Aligned with new processing model
#	
#	Revision 1.17  2007/07/05 08:35:53  simeon
#	July 5, 2007 - Jerome
#	
#	  o Normalization:
#	     - Fixed normalization of function calls whose expected type is
#	       xs:anyURI (wrongly casting to xs:string).
#	
#	  o Optimization:
#	     - Added clean-up optimization rule for inverse MapFrom/MapTo:
#	         Map{#x}(Map{[x:ID]}(Op)) == Op
#	
#	Revision 1.16  2007/02/28 18:48:06  mff
#	
#	February 28, 2007 - Mary (DXQ)
#	
#	  o Clean-up of Galax Server code
#	
#	    - New module Galax_server_util:
#	
#	      Factorized functionality shared by Galax_server, Galaxd, and
#	      Webgui into Galax_server_util.  Contains:
#	        * Server UDP defaults
#	        * Server-name management (maps from virtual to actual names)
#	        * Shared GUI utilities
#	
#	    - Moved file and directory management utilities into Gmisc.
#	
#	    - Moved DXQ options into Top_options.
#	
#	    - Processing_context contains "short" hostname and port of server.
#	
#	Revision 1.15  2006/08/16 20:30:08  simeon
#	August 16, 2006 - Jerome
#	
#	  o Release:
#	    - Set the version number to 0.6.8.
#	    - Added 0.6.8 entries in the documentation and Web sites.
#	    - Some minor fixes the documentation, notably updating the
#	      contributor's list.
#	    - Added some text for support for ULTF, XQueryP in the release
#	      notes.
#	    - Fixed the ns_usecase expected result to match the one generated
#	      by Galax. Namespace handling in constructors may be fixed at
#	      some point and result in subsequent changes.
#	    - Fixed usecase Makefile to not set monitoring on.
#	  o ULTF/XQuery!/XQueryP Trilogy:
#	    - Added -language ultf option for W3C Update Facility.
#	    - Added minimalistic tutorial in the documentation for how to run
#	      those.
#	    - Added examples in ./examples/extensions for each of the 3
#	      languages.
#	  o F&O:
#	    - Fixed fn:matches function to not take optional second and third
#	      arguments anymore.
#	
#	Revision 1.14  2006/05/25 16:23:42  mff
#	
#	May 25, 2006 - Mary
#	
#	  o Added implementation(s) for fs:item-sequence-to-node-sequence on
#	    DOM tree & XML-token cursors.
#	
#	    This function is only necessary for typing purposes, but if
#	    rewriting is off, it must be implemented as the identity function
#	    on both DOM trees and XML-token cursors.
#	
#	  o galax-parse command-line options:
#	
#	    -dm       => All phases through loading
#	    -validate => All phases through validation
#	
#	  o Documentation - some updates
#	
#	Revision 1.13  2006/05/15 15:20:36  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.12  2006/05/12 18:15:00  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.11  2006/04/17 18:10:40  car
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
LOCALPREFIX=..

include $(LOCALPREFIX)/config/Makefile.galax
include $(LOCALPREFIX)/website/Makefile.config

#########################################################################
# Section:	Local variables
# Description:
#		These variables are used for targets of this Makefile
###########################################################################
SOURCES=general.tex commandline.tex alignment.tex readme.tex install.tex \
release_notes.tex api.tex developers.tex license.tex tutorial.tex

HTMLFILES=\
alignment.html \
api.html \
commandline.html \
developers.html \
general.html \
index.html \
install.html \
manual001.html \
manual.css \
manual.html \
readme.html \
release_notes.html \
tutorial.html
#documents.html \


#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the regression targets
#  install::   This target does installation for C API files
#  uninstall:: This target uninstalls C API files
#  clean::     This target cleans files created in the all section
#########################################################################

#all:
all: manual.html

byte:

install: $(CONF_GALAX_MAN)
	$(CP) manual.pdf $(CONF_GALAX_MAN)
	for i in $(HTMLFILES); do ($(CP) $$i $(CONF_GALAX_MAN)); done
	for i in *.gif; do ($(CP) $$i $(CONF_GALAX_MAN)); done

install-website: $(CONF_GALAX_MAN)
	for i in $(HTMLFILES); do ($(CP) $$i $(WEBSITE)/doc); done
	for i in *.gif; do ($(CP) $$i $(WEBSITE)/doc); done

uninstall:
	$(RM) $(CONF_GALAX_MAN)/manual.pdf
	for i in $(HTMLFILES); do ($(RM) $(CONF_GALAX_MAN)/$$i); done
	for i in *.gif; do ($(RM) $(CONF_GALAX_MAN)/$$i); done

clean: 
	$(RM) manual.log manual.pdf *.aux *.haux *.htoc *.html

clobber::

realclean: clean
	$(RM) *.html manual.pdf

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

manual.html:	manual.pdf 
	hevea -exec /bin/date manual.tex
	hacha manual.html

manual.pdf:	manual.tex $(SOURCES) 
	pdflatex manual.tex
	pdflatex manual.tex

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


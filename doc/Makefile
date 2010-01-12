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

realclean: 
	$(RM) manual.log manual.pdf *.aux *.haux *.htoc *.html
clean: 
	$(RM) manual.log manual.pdf *.aux *.html

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

manual.html:	manual.pdf 
	hevea -fix -exec xxdate.exe manual.tex
	hacha manual.html

manual.pdf:	manual.tex $(SOURCES) 
	pdflatex manual.tex
	pdflatex manual.tex

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


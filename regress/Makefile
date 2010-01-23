#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.57 2007/05/02 20:27:20 mff Exp $ #

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

all: testconfig.xml

byte:

install: $(CONF_GALAX_REGRESS)
	$(SED) -e "s:XQUERYUNIT:$(CONF_XQUERYUNIT):" -e "s:GALAXUNIT:$(CONF_GALAXUNIT):" -e "s:REGRESS:$(CONF_GALAX_REGRESS):" testconfig-tmpl.xml > $(CONF_GALAX_REGRESS)/testconfig.xml
	$(CP) knownbugs.xml $(CONF_GALAX_REGRESS)
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):g" Makefile-inst > $(CONF_GALAX_REGRESS)/Makefile
	$(CP) testresults-W3C-reference.xml  $(CONF_GALAX_REGRESS)
	$(CP) testresults-Galax-reference.xml  $(CONF_GALAX_REGRESS)
	$(CP) diff.xq  $(CONF_GALAX_REGRESS)

uninstall:
	$(RM) $(CONF_GALAX_REGRESS)/testconfig.xml
	$(RM) $(CONF_GALAX_REGRESS)/knownbugs.xml
	$(RM) $(CONF_GALAX_REGRESS)/Makefile

clean: 
	$(RM) testconfig.xml
	$(RM) testresults-W3C.xml
	$(RM) testresults-Galax.xml
	$(RM) buildbot.diffs

clobber::

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################

testconfig.xml:	testconfig-tmpl.xml
	$(SED) -e "s:XQUERYUNIT:$(CONF_XQUERYUNIT):" -e "s:GALAXUNIT:$(CONF_GALAXUNIT):" -e "s:REGRESS:$(CONF_PACKAGE_SRC)/regress:" testconfig-tmpl.xml > testconfig.xml

tests: testconfig.xml
	$(LOCALPREFIX)/glx-test$(EXE) testconfig.xml

tests-gen: testconfig.xml
	$(LOCALPREFIX)/glx-test$(EXE) -generate-results testconfig.xml

w3c-diff:
	echo ""
	echo "******************************************"
	echo "*** Tests which differ from expected run: "
	echo "******************************************"
	echo ""
	$(LOCALPREFIX)/glx xquery -var unit=W3C diff.xq
	echo ""
	echo "******************************************"

galax-diff:
	echo ""
	echo "******************************************"
	echo "*** Tests which differ from expected run: "
	echo "******************************************"
	echo ""
	$(LOCALPREFIX)/glx xquery -var unit=Galax diff.xq
	echo ""
	echo "******************************************"

silent-diff:
	if test -f testresults-W3C.xml; then $(MAKE) w3c-diff; fi
	if test -f testresults-Galax.xml; then $(MAKE) galax-diff; fi

buildbot-diff: tests
	if test -f testresults-W3C.xml; then $(LOCALPREFIX)/glx -var unit=W3C diff.xq > buildbot.diffs; fi
	if test -f testresults-Galax.xml; then $(LOCALPREFIX)/glx -var unit=Galax diff.xq	>> buildbot.diffs; fi
	(j=`grep -l diff buildbot.diffs`; if test -n "$$j"; then exit 1; fi)

diff:
	$(MAKE) -s silent-diff


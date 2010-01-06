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
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax regression suite
# History:
#	$Log: Makefile,v $
#	Revision 1.57  2007/05/02 20:27:20  mff
#	
#	More buildbot testing
#	
#	Revision 1.56  2007/05/02 19:59:18  mff
#	
#	BuildBot TEST steps
#	
#	Revision 1.55  2007/05/02 19:14:08  mff
#	
#	Targets for buildbot regressions
#	
#	Revision 1.54  2007/02/12 21:15:33  simeon
#	February 12, 2007 - Jerome
#	
#	  o Code clean up:
#	     - Removed obsolete modules: Factorize_update, Factorize_util.
#	
#	  o Galax Test Suite:
#	     - Added a first version of the Galax test suite in the CVS
#	       repository. This currently contains a few tests for: plan
#	       stability of the use cases, and join detection tests.
#	
#	  o Testing:
#	     - Finalized support for comparing query plans in the test
#	       harness.
#	     - Added an option to automatically generate expected results in
#	       case the file for that expected result is missing.
#	
#	       Usage: [galax-test -generate-results] to generate results.
#	
#	  o Toplevel:
#	     - Added three plan conversion utilities for convenience.
#	        [xquery2plan] generates a plan from an XQuery. **
#	        [xquery2xmlplan] generates an XML plan from an XQuery.
#	        [xmlplan2plan] generates a plan from an XML plan.
#	
#	      **you can turn optimization on by writing -optimization on.
#	
#	  o Updates/Parsing:
#	     - Removed 'snap delete' etc. variants for the update
#	       operations. Fixing shift/reduce conflicts. (e.g., WITH could
#	       parse as either a do replace or a snap replace).
#	
#	       The changes are:
#	         MapFromItem[$x]{P1}(P2) --> Map{P1}(P2) where occurrence of $x
#	         in P1 is turned into ID.
#	         MapToItem{P1}(P2) --> Map{P1}(P2)
#	         INPUT --> ID
#	         snap { P } --> Snap(P), same for Delete,Replace,Insert,Rename.
#	
#	  o Pretty printer:
#	     - Fixed the algebraic pretty-printer to align with the current
#	       formalization of the algebra [Ghelli,Onose,Rose,Simeon].
#	
#	  o Compiler:
#	     - Consolidated name creation (variables, tuple fields, etc.)
#	       throughout the compiler. This is now handled by a proper module
#	       [Namespace_generate]. This is important notably for the
#	       regression tests to ensure stability of variable names accross
#	       separate sets of compilation.
#	
#	Revision 1.53  2007/02/01 22:08:53  simeon
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
#	Revision 1.52  2006/12/14 04:53:20  simeon
#	December 13, 2006 - Jerome
#	
#	  Current testing results on XQTS 1.0.2:
#	
#	  Feature Galax
#	  Minimal Conformance	          14497 / 114 / 14637  (99.1%)
#	  Optional Features
#	    Schema Import Feature	  0   	/ 0   / 174
#	    Schema Validation Feature	  0   	/ 0   / 25
#	    Static Typing Feature	  46  	/ 0   / 46
#	    Full Axis Feature	 	  130 	/ 0   / 130
#	    Module Feature	 	  0 	/ 0   / 32
#	    Trivial XML Embedding Feature 0 	/ 0   / 4
#	
#	  o Compilation:
#	     - Added support for separate -galax-regression option to indicate
#	       where the Galax Test Suite is installed.
#	     - Fixed circular dependency in optimization walker/judge modules,
#	       resulting from the latest reorg.
#	
#	  o Testing:
#	     - Fixed support for multiple test units, generating distinct
#	       files instead of overriding them. Fixed printing for multiple
#	       test units.
#	     - Added support for comparison of query plans.
#	
#	  o Updates:
#	     - Fixed 'do rename' to properly handle xs:string and xs:untyped
#	       values for the name expression.
#	
#	  o F&O:
#	     - Fixed fn:dateTime to support empty sequence parameters.
#	
#	  o Pragmas:
#	     - Now properly resolving Pragmas QNames.
#	     - Fixed serious parsing and lexing issues with pragmas (wrong
#	       lexical state created in some cases).
#	     - Fixed bug, now raising an error when there is no expression.
#	     - Added check that the pragma QName and content is separated by
#	       at least a whitespace character.
#	
#	  o Parsing:
#	
#	    Long over-due cleanup of the lexers. This fixes two major
#	    problems: (1) we now have keyword tables which makes for a more
#	    easily extensible parser, and smaller lexing tables.  (2) we don't
#	    rely on long keywords anymore which means we properly implement
#	    comments within keywords. [ Note that it's not completely true, we
#	    still have a few long keywords hanging around, notably those with
#	    QNames inside... This still need some work, but should get us
#	    going. ]
#	
#	     - Removing numerous unused lexing rules in some of the states,
#	       notably in the operator's state.
#	     - Moved part of the keywords used in prolog declarations from the
#	       operator's state to the declaration state.
#	     - Consolidated duplicated string literals and comments lexing
#	       rules to use the same lexer.
#	     - Added support for simple kind of look ahead in lexing handlers.
#	     - Added keyword tables for the operator, default, and namespace
#	       declaration states.
#	     - Removed (almost) all long keywords from the default,
#	       operators, and namespace declaration state.
#	     - Fixed bug, 'as' not recognized as a qname in the default state.
#	
#	Revision 1.51  2006/08/17 14:09:29  simeon
#	August 17, 2006 - Jerome
#	
#	  o Testing:
#	    - Added cleaner diff for expected vs. actual test results using
#	      XQuery.
#	
#	Revision 1.50  2006/08/16 19:55:52  mff
#	
#	Added reference test results to regress/
#	
#	Revision 1.49  2006/05/15 15:20:37  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.48  2006/05/12 18:15:01  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.47  2006/04/17 18:10:40  car
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
	$(LOCALPREFIX)/glx -var unit=W3C diff.xq
	echo ""
	echo "******************************************"

galax-diff:
	echo ""
	echo "******************************************"
	echo "*** Tests which differ from expected run: "
	echo "******************************************"
	echo ""
	$(LOCALPREFIX)/glx -var unit=Galax diff.xq
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


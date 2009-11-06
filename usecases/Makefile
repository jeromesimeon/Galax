#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.68 2007/07/11 19:58:52 simeon Exp $ #

#########################################################################
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax usecases
# History:
#	$Log: Makefile,v $
#	Revision 1.68  2007/07/11 19:58:52  simeon
#	July 10, 2007 - Jerome
#	
#	  o Pretty-printer:
#	     - Fixed pretty-printer for blocks inside function declarations.
#	     - Fixed pretty-printer for fixed attributes in elements! [See
#	       Nicola's thread]
#	
#	Revision 1.67  2007/05/16 15:32:13  mff
#	
#	
#	May 16, 2007 - Mary
#	
#	  MODULES and INTERFACES
#	  *** PLEASE READ THE WHOLE MESSAGE ***
#	
#	  o Deep changes to support separate module interfaces and module
#	    implementations.
#	
#	    - Compilation/evaluation now occur within context of a "compiled
#	      program".  (In new module code_selection/Compiled_program_units)
#	
#	      A compiled program consists of imported module interfaces,
#	      imported library modules, and an optional main module.
#	
#	    - All program units (statements,prologs,modules) are compiled in
#	      context of a compiled_program unit.
#	
#	    - New pre-processing phase (before normalization), computes
#	      transitive closure of imported library modules.  Used to
#	      populate compiled_program unit.
#	
#	    - Recursive module imports (as per standard) are prohibited.
#	
#	   o Changes to ASTs:
#	
#	     We distinguish between "external" functions/vars (which are
#	     defined in external programming environment) from "imported"
#	     functions/vars (which are defined in imported XQuery modules)
#	
#	     New kinds of declarations:
#	     Xquery_ast : EFunctionInterface and EVarInterface
#	     Xquery_core_ast : CEFunctionInterface and CEVarInterface
#	     Xquery_algebra_ast : AOEFunctionInterface and AOEVarDeclInterface
#	
#	   o Importing interfaces and modules
#	
#	     Module interfaces are imported during normalization.  Variable
#	     and function declarations in the Core rep of the imported
#	     interface are merged into the Core rep of the importing module.
#	
#	     If a module does not have an interface, an interface is created
#	     for it during the pre-processing stage.
#	
#	   o New interface stdlib/pervasive.xqi replaces stdlib/pervasive.xq
#	
#	     - Removed stdlib/pervasive.xq
#	     - NB: new file suffix ".xqi" for module interfaces.
#	
#	   o Code selection
#	
#	     References to imported variables and functions are resolved
#	     during code selection of the importing module.
#	
#	     The code_selection_context for a module points to the
#	     code_selection_contexts of the modules that it imports to resolve
#	     references to imported variables/functions.
#	
#	   o What works:
#	
#	     - Usecase, XQuery testsuite, and Galax regressions all pass
#	
#	   o What doesn't work:
#	
#	     - examples/caml_api/test.ml
#	     - C or Java APIs
#	     - Any DXQ program
#	
#	   o TODO:
#	
#	     - Physical types for external and imported variables: Right now,
#	     they are completely materialized XML values.
#	
#	Revision 1.66  2007/05/02 20:27:20  mff
#	
#	More buildbot testing
#	
#	Revision 1.65  2007/05/02 19:59:18  mff
#	
#	BuildBot TEST steps
#	
#	Revision 1.64  2007/05/02 19:14:08  mff
#	
#	Targets for buildbot regressions
#	
#	Revision 1.63  2007/02/12 21:15:35  simeon
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
#	Revision 1.62  2007/02/01 22:08:55  simeon
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
#	Revision 1.61  2006/08/16 18:33:32  mff
#	August 16, 2006 - Mary
#	
#	  o Configure script
#	    - Sets $ocaml_home if not set by default
#	
#	  o Code_selection_context:
#	    - Added enter/exit_closure_context
#	
#	  o Cs_code_selection_top
#	    Galapi/galax
#	    Procmod_compiler
#	    Procmod_phases
#	    Procmod_types
#	
#	    Cs_code_selection_top.code_selection_statement now correctly
#	    returns updated code_selection_context.  Previously, it was
#	    dumped.
#	
#	    Corresponding changes in modules listed above to propagate and
#	    return updated code_selection_context.
#	
#	  o Cs_code_typing_top:
#	    type annotation of top-level expression stored as a global
#	    annotation so available to caller.
#	
#	  o Cs_util_coercion:
#	    Added missing coercion functions
#	
#	  o Code_binding:
#	    Tuple fields contain physical_xml_values, not physical_values:
#	    minor changes to support this change
#	
#	  o Code_execute:
#	    Added support for shipping plans that return tables/tuple-streams
#	
#	  o Code_util_materialize:
#	    Changed names of functions to reflect type of materialization,
#	    e.g., materialize_cursor_to_dom_value_array
#	
#	    Many files updated to reflect name change:
#	    code_group_order.ml
#	    code_hash_join.ml
#	    code_join.ml
#	    code_nestedloop.ml
#	    code_sort_join.ml
#	    code_tuple.ml
#	
#	  o Parse_xquery.mly:
#	    Fixed long-standing bug: EOF missing from end of library and main
#	    modules.
#	
#	  o Website/demo/querycgi.ml : alignment
#	
#	August 16, 2006 - Mary (DXQ)
#	
#	  o DNS examples:
#	    Changed name of module interface to dns.xqi
#	
#	  o Optimization_dxq:
#	    Working on DXQ opts
#	
#	  o Planio/*:
#	    Added support for serializing and parsing.
#	    ***All these files need to be reorganized.  They are a mess***
#	
#	  o Galax_server: fixed error-return XML
#	
#	  o Galaxd: yet another debug log bug
#	
#	Revision 1.60  2006/05/15 15:20:37  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.59  2006/05/12 18:15:01  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.58  2006/04/17 18:10:41  car
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
# Section:	Local variables
# Description:
#		These variables are used for targets of this Makefile
###########################################################################

BINDIR=..
TYPE_KIND=weak
USECASES= ns parts rel seq sgml string tree xmp

ifdef OCAMLOPT
BINEXT=$(OPT)
else
BINEXT=$(BYTE)
endif

DOCFILES=\
auction.xml \
bids.xml \
book1.xml \
books.xml \
company-data.xml \
company.dtd \
herpo.xsd \
hispo.xsd \
items.xml \
parts.xml \
prices.xml \
report1.xml \
sgml.xml \
string.xml \
users.xml \
xmark.xml \
xmpbib.xml \
xmpreviews.xml

#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target does nothing in the build environment
#  install::   This target does installation for the usecases
#  uninstall:: This target uninstalls the usecases
#  clean::     This target cleans files created in the all section
#########################################################################

all:

byte:

install: $(CONF_GALAX_USECASES) $(CONF_GALAX_USECASES)/docs
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):" Makefile-inst > $(CONF_GALAX_USECASES)/Makefile
	for i in $(USECASES) xmark; do ($(CP) $${i}_context.xq $${i}_usecase.xq $${i}_usecase.expect $(CONF_GALAX_USECASES)); done
	for i in $(DOCFILES); do ($(CP) docs/$$i $(CONF_GALAX_USECASES)/docs); done

uninstall:
	$(RM) $(CONF_GALAX_USECASES)/Makefile
	for i in $(USECASES) xmark; do ($(RM) $(CONF_GALAX_USECASES)/$${i}_context.xq $(CONF_GALAX_USECASES)/$${i}_usecase.xq $(CONF_GALAX_USECASES)/$${i}_usecase.expect); done
	for i in $(DOCFILES); do ($(RM) $(CONF_GALAX_USECASES)/docs/$$i); done

clean:
	cd xqupdate; $(MAKE) clean
	$(RM) *_pretty.*
	$(RM) *_pretty_core.*
	$(RM) *.xml *.cmp
	$(RM) *.mon
	$(RM) buildbot.diffs

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

tests:
	$(MAKE) clean
	$(MAKE) -s run-orig
	$(MAKE) -s bootstrap

buildbot-diff:	
	$(MAKE) clean
	$(MAKE) run-orig > buildbot.diffs
	(j=`grep -l diff buildbot.diffs`; if test -n "$$j"; then exit 1; fi)

bootstrap:
	$(MAKE) -s compile
	$(MAKE) -s run-compiled

bootstrap-core:
	$(MAKE) -s compile-core
	$(MAKE) -s run-compiled-core

bootstrap-double-core:
	$(MAKE) -s compile-double-core
	$(MAKE) -s run-compiled-double-core

%_usecase.xml: %_usecase.xq %_context.xq
	($(BINDIR)/galax-run$(BINEXT)  -serialize wf -static-typing on -typing ${TYPE_KIND} -context $*_context.xq $*_usecase.xq >$*_usecase.xml; \
	$(MAKE) -k -s check T=$*_usecase E=$*_usecase; )

%_type.xml: %_usecase.xq %_context.xq
	(echo "$*: none"; time $(BINDIR)/galax-run$(BINEXT) -static-typing on -typing none -context $*_context.xq $*_usecase.xq -dynamic off; \
	echo "$*: weak"; time $(BINDIR)/galax-run$(BINEXT) -static-typing on -typing weak -context $*_context.xq $*_usecase.xq -dynamic off)

#	(time $(BINDIR)/galax-run$(BINEXT) -monitor-time on -output-monitor $*.mon -serialize wf -static-typing on -typing none -context $*_context.xq $*_usecase.xq -dynamic off; \
#	time $(BINDIR)/galax-run$(BINEXT) -monitor-time on -output-monitor $*.typemon -serialize wf -static-typing on -typing weak -context $*_context.xq $*_usecase.xq -dynamic off)

run-type:
	@echo "";\
        echo "Static typing:" ;\
	echo "=============" ;\
	echo "";\
	(for t in $(USECASES) xmark; do \
	make -k -s $${t}_type.xml ; \
	done)

run-xmark-type:
	@echo "";\
        echo "Static typing:" ;\
	echo "=============" ;\
	echo "";\
	(for t in Q*.xq; do \
	echo "$${t}: none"; time $(BINDIR)/galax-run$(BINEXT) -static-typing on -typing none -context xmark_context.xq $${t} -dynamic off; \
	echo "$${t}: weak"; time $(BINDIR)/galax-run$(BINEXT) -static-typing on -typing weak -context xmark_context.xq $${t} -dynamic off; \
	done)

run-orig:
	@echo "";\
        echo "Original queries:" ;\
	echo "=================" ;\
	echo "";\
	(for t in $(USECASES) xmark; do \
	make -k -s $${t}_usecase.xml ; \
	done)

#	$(BINDIR)/galax-run$(BINEXT) -treejoin-log twig  -serialize wf -context $${t}_context.xq $${t}_usecase.xq >$${t}_usecase.xml; \

run-compiled:
	@echo;\
        echo "Pretty printed queries:" ;\
	echo "=======================" ;\
	echo "";\
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/galax-run$(BINEXT) -serialize wf -optimization on $${t}_usecase_pretty.xq > $${t}_usecase_pretty.xml; \
	$(MAKE) -k -s check T=$${t}_usecase_pretty E=$${t}_usecase; \
	done)

run-compiled-core:
	@echo "";\
        echo "Pretty printed Core:" ;\
	echo "====================" ;\
	echo "";\
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/galax-run$(BINEXT) -factorization off -execute normalized -serialize wf -optimization on $${t}_usecase_pretty_core.xq > $${t}_usecase_pretty_core.xml; \
	$(MAKE) -k -s check T=$${t}_pretty_core E=$${t}_usecase; \
	done)

run-logical-plan: 
	@echo "";\
        echo "Execute Logical Plans:" ;\
	echo "=====================" ;\
	echo "";\
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/galax-run$(BINEXT) -dynamic off -print-plan-kind xml -output-logical-plan $${t}_plan.xml -print-logical-plan on -context $${t}_context.xq $${t}_usecase.xq; \
	$(BINDIR)/galax-run$(BINEXT) -execute logical $${t}_plan.xml  >$*_usecase.xml; \
	$(MAKE) -k -s check T=$*_usecase E=$*_usecase; \
	done)

compile:
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/galax-compile$(BINEXT) -print-optimized-plan off -print-prolog on -print-expr on -context $${t}_context.xq $${t}_usecase.xq > $${t}_usecase_pretty.xq; \
	done)

compile-core:
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/galax-compile$(BINEXT) -print-optimized-plan off -print-prolog on -print-rewritten-expr on -context $${t}_context.xq $${t}_usecase.xq > $${t}_usecase_pretty_core.xq; \
	done)

compile-double-core:
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/galax-compile$(BINEXT) -execute normalized -print-optimized-plan off -print-prolog on -print-normalized-expr on $${t}_usecase_pretty_core.xq > $${t}_usecase_double_pretty_core.xq; \
	done)

gen: clean	
	(wd=`pwd`; \
         for t in $(USECASES); \
         do echo $$t; \
           /bin/rm -f $${wd}/$${t}_usecase.xq $${wd}/$${t}_context.xq $${wd}/$${t}_usecase.expect;  \
           cp $(XQUERYUNIT)/tests/xquery-usecases-oct2004/$$t/context.xq $${wd}/$${t}_context.xq; \
           cd $(XQUERYUNIT)/tests/xquery-usecases-oct2004/$$t/xquery/; \
           for f in Q*.xq; \
	     do cat $${f}  >> $${wd}/$${t}_usecase.xq ; \
             echo "" >> $${wd}/$${t}_usecase.xq ;  \
             echo ";" >> $${wd}/$${t}_usecase.xq ;  \
	   done; \
           cd $(XQUERYUNIT)/tests/xquery-usecases-oct2004/$$t/expect/; \
           for f in Q*.xml; \
	     do cat $${f}  >> $${wd}/$${t}_usecase.expect ; \
	   done; \
	   cp $(XQUERYUNIT)/tests/xquery-usecases-oct2004/$$t/xquery/docs/*.xml $${wd}/docs;  \
         done; \
         echo xmark; \
         /bin/rm -f $${wd}/xmark_usecase.xq $${wd}/xmark_context.xq $${wd}/xmark_usecase.expect;  \
         cp $(XQUERYUNIT)/tests/xmark-oct2004/context.xq $${wd}/xmark_context.xq ; \
         cd $(XQUERYUNIT)/tests/xmark-oct2004/xquery/; \
           for f in Q*.xq; \
	   do cat $${f}  >> $${wd}/xmark_usecase.xq ; \
           echo "" >> $${wd}/xmark_usecase.xq ;  \
           echo ";" >> $${wd}/xmark_usecase.xq ;  \
	   done; \
           cd $(XQUERYUNIT)/tests/xmark-oct2004/expect/; \
           for f in Q*.xml; \
           do cat $${f}  >> $${wd}/xmark_usecase.expect ; \
	   done; \
	 cp $(XQUERYUNIT)/tests/xmark-oct2004/xquery/docs/*.xml $${wd}/docs;  \
        )

$(CONF_GALAX_USECASES)/docs:
	$(MKDIR) $(CONF_GALAX_USECASES)/docs

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


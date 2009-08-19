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
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax OCaml API examples.
# History:
#	$Log: Makefile,v $
#	Revision 1.35  2007/05/16 15:32:10  mff
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
#	Revision 1.34  2007/02/01 22:08:46  simeon
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
#	Revision 1.33  2006/05/15 15:20:36  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.32  2006/05/12 18:15:00  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.31  2006/04/17 18:10:40  car
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
LOCALPREFIX=../..

include $(LOCALPREFIX)/config/Makefile.galax

#########################################################################
# Sub-Section:	Targets
# Description:
#		These are the configuration-dependent targets built by "all"
###########################################################################
BYTETARGETS=\
example$(BYTE) \
test$(BYTE)

OPTTARGETS=\
example$(OPT) \
test$(OPT)

TESTS=runexample runtest

TARGETS=$(BYTETARGETS)
ifdef OCAMLOPT
TARGETS+=$(OPTTARGETS)
TESTS+=runoptexample runopttest
endif

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

all: $(TARGETS)

byte:	$(BYTETARGETS)

tests:	$(TESTS)

install: $(CONF_GALAX_EXAMPLES)/caml_api
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):g" Makefile-inst > $(CONF_GALAX_EXAMPLES)/caml_api/Makefile
	$(CP) example.ml $(CONF_GALAX_EXAMPLES)/caml_api
	$(CP) example.expect $(CONF_GALAX_EXAMPLES)/caml_api
	$(CP) test.ml $(CONF_GALAX_EXAMPLES)/caml_api
	$(CP) test.expect $(CONF_GALAX_EXAMPLES)/caml_api

uninstall:
	$(RM) $(CONF_GALAX_EXAMPLES)/caml_api/Makefile
	$(RM) $(CONF_GALAX_EXAMPLES)/caml_api/example.ml
	$(RM) $(CONF_GALAX_EXAMPLES)/caml_api/example.expect
	$(RM) $(CONF_GALAX_EXAMPLES)/caml_api/test.ml
	$(RM) $(CONF_GALAX_EXAMPLES)/caml_api/test.expect

clean:
	$(RM) $(TARGETS)
	$(RM) *.cmx *.cmo *.o

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

$(CONF_GALAX_EXAMPLES)/caml_api:
	$(MKDIR) $(CONF_GALAX_EXAMPLES)/caml_api

example$(BYTE):	example.cmo
	$(OCAMLC) -custom -linkall $(OCAMLC_FLAGS) -o $@ $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) example.cmo

example$(OPT):	example.cmx
	$(OCAMLOPT) -linkall $(OCAMLOPT_FLAGS) -o $@ $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) example.cmx

test$(BYTE):	test.cmo
	$(OCAMLC) -custom -linkall $(OCAMLC_FLAGS) -o $@ $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) test.cmo

test$(OPT):	test.cmx
	$(OCAMLOPT) -linkall $(OCAMLOPT_FLAGS) -o $@ $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) test.cmx

runexample:	example$(BYTE)
	$(RM) example.out
	./example$(BYTE) > example.out
	if [[ -z "`cmp example.out example.expect`" ]]; then echo "examples/caml_api/example$(BYTE) passed"; else $(MAKE) TEST=example$(BYTE) error; fi;

runtest:	test$(BYTE)
	$(RM) test.out
	./test$(BYTE) > test.out
	if [[ -z "`cmp test.out test.expect`" ]]; then echo "tests/caml_api/test$(BYTE) passed"; else $(MAKE) TEST=test$(BYTE) error; fi;

runoptexample:	example$(OPT)
	$(RM) optexample.out
	./example$(OPT) > optexample.out
	if [[ -z "`cmp optexample.out example.expect`" ]]; then echo "examples/caml_api/example$(OPT) passed"; else $(MAKE) TEST=example$(OPT) error; fi;

runopttest:	test$(OPT)
	$(RM) opttest.out
	./test$(OPT) > opttest.out
	if [[ -z "`cmp opttest.out test.expect`" ]]; then echo "tests/caml_api/test$(OPT) passed"; else $(MAKE) TEST=test$(OPT) error; fi;

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currenlty no post-includes for this Makefile
#########################################################################


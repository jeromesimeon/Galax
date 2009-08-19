#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.354 2008/02/21 21:41:21 simeon Exp $ #

#########################################################################
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for Galax that relies on the output
#		of a configuration script.  It is an attempt to clean up the
#		existing build process and make it more amenable to packaging
#		systems like GODI.
# History:
#	$Log: Makefile,v $
#	Revision 1.354  2008/02/21 21:41:21  simeon
#	February 21, 2008 - Jerome
#	
#	  o Debian:
#	     - Folded in Stefano's patches.
#	
#	  o Documentation:
#	     - Re-enabled documentation generation.
#	     - Fixed some formatting issues.
#	
#	Revision 1.353  2007/09/27 19:19:28  simeon
#	September 20, 2007 - Jerome
#	
#	** Please run configure again due to changes to detect Camomile's version **
#	
#	  o Galax Compilation:
#	     - Fixes to ./configure to support camomile's version.
#	     - Upgraded to Ocaml 3.10
#	       (changes in compilation due to new camlp4)
#	     - Added support for Camomile 0.7.* on top of Camomile 0.6.
#	       (changes due to new library names)
#	     - Now forcing use of Netsys, coming with latest ocamlnet.
#	
#	  o Join optimization:
#	     - Another fix to the compiler, to properly clean-up selec
#	       clause for join detection.
#	
#	Revision 1.352  2007/08/09 20:21:22  simeon
#	August 9, 2007 - Jerome
#	
#	  o XQueryX:
#	    - Added support for module interfaces in xqueryx2xquery.
#	
#	  o Interfaces:
#	    - Fixed pretty-printing of module interfaces.
#	
#	  o Galax Compilation:
#	    - Added missing dependencies for some target executables to the Galax
#	      library.
#	
#	Revision 1.351  2007/08/01 18:06:30  simeon
#	August 1, 2007 - Jerome
#	
#	  o XQueryX: First-cut support for XQueryX with trivial embedding
#	    in Galax. It supports the following features:
#	
#	     (1) Compiling XQueryX to XQuery using a command line tool called
#	         xqueryx2xquery.
#	
#	     (2) Running XQueryX with galax-run with the flag '-syntax
#	         xqueryx'.
#	
#	     (3) Embedding XQueryX expressions directly within XQuery.
#	
#	  (1) and (2) work on whole modules, while (3) only works with
#	  expressions since it has to be integrated with the rest of the
#	  grammar.
#	
#	  o Processing model:
#	     - Fixed ugly bug in the way parse handlers were built. Now
#	       properly taking configuration into account.
#	
#	Revision 1.350  2007/07/31 17:34:08  simeon
#	July 31, 2007 - Jerome
#	
#	  o Parsing:
#	     - Fixed another lurking bug in the lexing of sequence types,
#	       transitioning to the wrong state... (Was tripped by e.g.,
#	       instance of or typeswitch inside a constructor).
#	
#	       [Replaced old hack by better working new hack...sigh
#	
#	        Queries to test:
#	        <a>{1 instance of item()}</a>
#	        <a>{1 instance of item()*}</a>
#	        <a>{1 instance of item()(::)*}</a>
#	        every $a as item()* in (1, 2), $b as item()* in $a satisfies $b]
#	
#	  o Prolog:
#	     - Fixed another lurking bug in cyclic variable dependency
#	       checking (not clearing up the previous variable table in
#	       parallel branches of the dependency graph).
#	
#	  o XQueryX Trivial Embedding:
#	     - Started prepare for it. Added hook in normalization.
#	
#	Revision 1.349  2007/07/23 18:15:19  ndonose
#	fixed makefile options for zerod
#	
#	Revision 1.348  2007/07/21 17:46:07  ndonose
#	July 21, 2007 - Nicola
#	
#	  o Workflows:
#	    First version of an HTTP server providing an interface for
#	    ProjectZero workflows and acting as proxy towards the DXQ
#	    server running the workflow script.
#	    The current version does not support yet receive activities
#	    that are not enabled from the beginning.
#	
#	Revision 1.347  2007/07/18 16:54:18  simeon
#	July 18, 2007 - Jerome
#	
#	  Feature Galax
#	  Minimal Conformance	          14528 / 96 / 14637  (99.3%)
#	  Optional Features
#	    Schema Import Feature	  0   	/ 0   / 174
#	    Schema Validation Feature	  0   	/ 0   / 25
#	    Static Typing Feature	  46  	/ 0   / 46
#	    Full Axis Feature	 	  130 	/ 0   / 130
#	    Module Feature	 	  0 	/ 0   / 32
#	    Trivial XML Embedding Feature 0 	/ 0   / 4
#	
#	  o WSDL:
#	     - Removing wsdl_load.ml at cleanup-time, since it's being
#	       generated.
#	
#	  o Prolog:
#	     - Added check for cyclic definitions of global variable in
#	       modules.
#	
#	  o Testing:
#	     - Added group testing for the XQuery Appendices.
#	
#	Revision 1.346  2007/07/13 18:24:42  mff
#	July 13, 2007 - Mary
#	
#	   o Xquery_algebra_ast.mli + all files that depends on Algebra AST
#	     - Changes names of AST nodes for imported variables & functions.
#	       Conflated interfaces with imported
#	
#	   o Changed Pervasives _back_ to an XQuery module from an interface,
#	     because it really is a module whose functions are built-in.
#	     Sorry for churn on this.
#	
#	     - Removed stdlib/pervasive.xqi
#	     - Added glx:string_of_item
#	
#	   o Planio
#	     - Fixed bug in parsing of ElementKind and AttributeKind
#	
#	   o Code_execute
#	
#	     - Temporarily disabled optimization of Execute expressions to
#	       local server, which simply evaluate the remote expression
#	       locally, because there is an insidious bug when its called.
#	
#	   o Parse_xquery
#	     - Changed parsing of interfaces so that variables and functions
#	       are correctly tagged as coming from interfaces.  Previously,
#	       did this with an ugly hack in normalization.
#	
#	   o Galaxd & Galax_server
#	     - Added exception-handler argument to tcp_server and udp_server
#	       so that internal errors and async errors can be reported to the
#	       Gui.
#	
#	Revision 1.345  2007/06/28 15:17:30  ndonose
#	June 28, 2007 - Nicola
#	
#	  o WSDL: Updated the WSDL parser to make it work again with the
#	    latest changes in Galax.
#	
#	  o Removed wsdl_load.ml from the repository, as it is supposed to
#	    be generated from wsdl_load.mlp
#	
#	Revision 1.344  2007/05/16 15:32:07  mff
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
#	Revision 1.343  2007/05/02 19:14:08  mff
#	
#	Targets for buildbot regressions
#	
#	Revision 1.342  2007/05/01 21:03:06  mff
#	
#	fast-regression added to Makefile
#	
#	Revision 1.341  2007/05/01 20:32:24  mff
#	
#	Added usecases: target for automated testing
#	
#	Revision 1.340  2007/03/21 15:52:52  mff
#	March 21, 2007 - Mary
#	
#	  o Galax_server:
#	    - Removed log of query-response size that was growing without
#	      bound!
#	    - Drip, drip, drip: other small memory leaks abound...
#	
#	Revision 1.339  2007/02/12 21:15:33  simeon
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
#	Revision 1.338  2007/02/01 22:08:45  simeon
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
#	Revision 1.337  2007/01/18 20:13:23  mff
#	
#	
#	January 18, 2007 - Mary
#	
#	  o PlanIO
#	    - Paired down XML rep of plans to something reasonable.
#	      TODO: Omit function signatures from serialization of Call ops
#	
#	  o Tail recursion
#	
#	    - Modified Call operators in Core/Algebra ASTs to include tag for
#	      tail-recursive function calls.
#	
#	      XQueryP while loops, compiled into tail-recursive calls, are
#	      tagged.
#	
#	    - Still working on changes to interpreter to evaluate tail-recursive
#	      XQuery function calls in O'Caml while loops (which are tail
#	      recursive!)
#	
#	  o Webgui [DXQ]
#	
#	    - Moved XHTML pretty printing into GUI.
#	
#	Revision 1.336  2006/12/08 15:18:53  trevor
#	  o GUI changed to display asynchronous queries and jobs differently
#	    than synchronous queries/jobs.
#	  o Galaxd submits jobs to GUI using UDP
#	
#	Revision 1.335  2006/10/13 16:27:15  mff
#	October 13, 2006 - Mary
#	  o Put glx:deep-distinct() back.
#	
#	Revision 1.334  2006/10/03 18:46:47  mff
#	
#	
#	October 3, 2006 - Mary
#	
#	  o Extricated threads module/options from all modules except Galaxd.
#	
#	    Galax_server is now a functor, which takes the "kind" of server
#	    module (Thread or Forked) as an argument.
#	
#	    Extended processing context to include the server function to
#	    invoke a remote query, because it depends on the kind of
#	    Galax_server.  The "toplevel" program (galaxd) determines what
#	    kind of server to use.
#	
#	    The server's query function must be available to the Execute
#	    operator, so I extended the Processing_context to include it.
#	
#	Revision 1.333  2006/09/22 02:15:40  mff
#	September 21, 2006 - Mary
#	 o Makefile fixes
#	
#	Revision 1.332  2006/09/21 16:52:33  mff
#	**********************************
#	*** You must re-run configure. ***
#	**********************************
#	
#	September 21, 2006 - Mary
#	
#	 o Removed "-with-galaxd" option from configure script.  Switch no
#	   longer necessary.  Makefile now compiles & links all top-level
#	   targets appropriately: galaxd links with galax-threads.cm*a and all
#	   other targets with galax.cm*a
#	
#	   Removed toplevel/top_server.ml from CVS b/c it's generated.
#	
#	Revision 1.331  2006/09/13 19:04:27  mff
#	**********************************
#	*** You must re-run configure. ***
#	**********************************
#	
#	September 13, 2006 - Mary
#	
#	 o Added "-with-galaxd" option to configure script (default without)
#	
#	   -with-galaxd (re)compiles all modules with threads enabled,
#	    link with threads library, and creates galaxd.
#	
#	   !!NB!!: All top-level executables will be thread-enabled and
#	   therefore *RUN SLOW*.  Only use -with-galaxd if you really need it.
#	
#	
#	 o Added toplevel/{top,thread,unix}_server.  Makefile selects
#	   appropriate server based on config options.
#	
#	Revision 1.330  2006/08/21 14:51:13  trevor
#	 o Added the web interface for galaxd.
#	
#	Revision 1.329  2006/08/16 20:30:08  simeon
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
#	Revision 1.328  2006/08/14 16:02:17  simeon
#	*** empty log message ***
#	
#	Revision 1.327  2006/08/12 18:28:04  mff
#	
#	Removed xml-diff target, which was added incorrectly.
#	
#	Revision 1.326  2006/08/11 20:09:35  kristi
#	August 11, 2006 - Kristi (Distributed XQuery)
#	
#	 o Modified galax-parse.ml, top_options.ml, top_config.ml, top_config.mli
#	   to support -diff option (compares 2 XML files), so can use galax-parse -diff to diff 2 XML
#	   files
#	
#	o Added preliminary test cases for DXQ in examples/dxq/test/q
#	  along with 2 programs, run-tests and generate-tests
#	  run-tests is a shell script that calls galax-parse -diff to check
#	  if tests passed.  generate-tests is a perl script that
#	  generates null-ary test cases
#	
#	Revision 1.325  2006/06/15 23:59:58  simeon
#	June 15, 2006 - Jerome
#	
#	 o Updates:
#	    - Fixed bug in physical typing of insert/replace/rename.
#	
#	    - Added top-level flag to select the language support (between
#	      XQuery 1.0 and XQuery!). Default is XQuery 1.0. Please use the
#	      following option to turn XQuery! support on:
#	
#	       -language xquerybang
#	
#	      Notes:
#	        * This only currently work with optimization off!! Something
#	          got broken in the safety checks of the optimizer with all
#	          the work we are going on it...
#	        * This also turning static typing off since it is not
#	          implemented (and wouldn't be safe!).
#	
#	Revision 1.324  2006/05/16 23:33:59  simeon
#	May 16, 2006 - Jerome
#	
#	  *** Test suite change: we now run 0.9.0.
#	     WARNINGS:
#	       - 0.8.6 will NOT run, because of xdt: to xs: namespace changes.
#	       - several Galax bugs are being exposed by 0.9.0 which are not
#	         fixed yet. ***
#	
#	  o Printing:
#	     - Fixed issue with flushing of formatters in pretty-printing
#	       stubs. (Gmisc module).
#	
#	  o Alignment:
#	     - Removed xdt: namespace, now replaced by xs: namespace.
#	     - Added support for 'encoding' option in the XQuery version
#	       declaration.
#	     - Implemented fn:codepoint-equal() function.
#	     - Fixed bug in parsing for 'castable as' expression with an
#	       optional type.
#	
#	  o Arithmetics:
#	     - Fixed long-standing bug in arithmetics of dates and time not
#	       dealing with the empty sequence properly, both in the type
#	       signatures and evaluation code.
#	     - Short-circuits the evaluation in the case one of the operand is
#	       the empty sequence.
#	
#	  o Constructor functions:
#	     - Constructor functions are now properly normalized to cast based
#	       on whether the function name derives from xs:anyAtomicType,
#	       instead of just as being in the xs: namespace.
#	     - Constructor functions now properly normalized to a cast
#	       expression with an optional type.
#	
#	  o Testing:
#	     - Fixed test harness to handle cases where the error comes out of
#	       serialization rather than evaluation.
#	
#	Revision 1.323  2006/05/15 15:20:36  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.322  2006/05/12 18:15:00  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.321  2006/05/11 16:54:58  simeon
#	May 11, 2006 - Jerome
#	
#	  ** The test-suite should work again **
#	
#	  o XML Parsing:
#	     - Fixed bug introduced last week about resolution of attributes
#	       in the default namespace, affecting testing.
#	
#	Revision 1.320  2006/04/26 14:25:04  car
#	April 26, 2006 - Chris Rath
#	  o Added toplevel command sources to the .depend target.
#	  o Updated the configure script to support -enable-profiling option.
#	  o Updated Makefile.galax to support profiling.
#	
#	Revision 1.319  2006/04/17 18:10:40  car
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
#				applications in the build environment
#########################################################################
LOCALPREFIX=.

include $(LOCALPREFIX)/config/Makefile.galax

GALAX_LIB_TARGETS=$(GALAX_LIB)

ifdef OCAMLOPT
GALAX_LIB_TARGETS+=$(GALAX_OPTLIB)
endif

# Galax top-level apps

TARGETNAME1=galax-run
TARGET1=$(TARGETNAME1)$(EXE)
OPTTARGET1=$(TARGETNAME1)$(OPT)
BYTETARGET1=$(TARGETNAME1)$(BYTE)
OPTPROFTARGET1=$(TARGETNAME1).optprof
TARGETOBJS1= toplevel/$(TARGETNAME1).cmo

TARGETNAME2=galax-mapschema
TARGET2=$(TARGETNAME2)$(EXE)
OPTTARGET2=$(TARGETNAME2)$(OPT)
BYTETARGET2=$(TARGETNAME2)$(BYTE)
TARGETOBJS2=toplevel/$(TARGETNAME2).cmo

TARGETNAME3=galax-parse
TARGET3=$(TARGETNAME3)$(EXE)
OPTTARGET3=$(TARGETNAME3)$(OPT)
BYTETARGET3=$(TARGETNAME3)$(BYTE)
TARGETOBJS3=toplevel/$(TARGETNAME3).cmo

TARGETNAME4=galax-mapwsdl
TARGET4=$(TARGETNAME4)$(EXE)
OPTTARGET4=$(TARGETNAME4)$(OPT)
BYTETARGET4=$(TARGETNAME4)$(BYTE)
TARGETOBJS4=toplevel/$(TARGETNAME4).cmo

TARGETNAME5=galax-project
TARGET5=$(TARGETNAME5)$(EXE)
OPTTARGET5=$(TARGETNAME5)$(OPT)
BYTETARGET5=$(TARGETNAME5)$(BYTE)
TARGETOBJS5=toplevel/$(TARGETNAME5).cmo

TARGETNAME6=xquery2soap
TARGET6=$(TARGETNAME6)$(EXE)
OPTTARGET6=$(TARGETNAME6)$(OPT)
BYTETARGET6=$(TARGETNAME6)$(BYTE)
TARGETOBJS6=toplevel/$(TARGETNAME6).cmo

TARGETNAME7=galax-compile
TARGET7=$(TARGETNAME7)$(EXE)
OPTTARGET7=$(TARGETNAME7)$(OPT)
BYTETARGET7=$(TARGETNAME7)$(BYTE)
TARGETOBJS7= toplevel/$(TARGETNAME7).cmo

TARGETNAME8=galax-test
TARGET8=$(TARGETNAME8)$(EXE)
OPTTARGET8=$(TARGETNAME8)$(OPT)
BYTETARGET8=$(TARGETNAME8)$(BYTE)
TARGETOBJS8= regress/$(TARGETNAME8).cmo

TARGETNAME9=webgui
TARGET9=$(TARGETNAME9)$(EXE)
OPTTARGET9=$(TARGETNAME9)$(OPT)
BYTETARGET9=$(TARGETNAME9)$(BYTE)
TARGETOBJS9=toplevel/$(TARGETNAME9).cmo

TARGETNAME10=galaxd
TARGET10=$(TARGETNAME10)$(EXE)
OPTTARGET10=$(TARGETNAME10)$(OPT)
BYTETARGET10=$(TARGETNAME10)$(BYTE)
TARGETOBJS10=toplevel/$(TARGETNAME10).cmo

TARGETNAME11=xquery2plan
TARGET11=$(TARGETNAME11)$(EXE)
OPTTARGET11=$(TARGETNAME11)$(OPT)
BYTETARGET11=$(TARGETNAME11)$(BYTE)
TARGETOBJS11= toplevel/$(TARGETNAME11).cmo

TARGETNAME12=xquery2xmlplan
TARGET12=$(TARGETNAME12)$(EXE)
OPTTARGET12=$(TARGETNAME12)$(OPT)
BYTETARGET12=$(TARGETNAME12)$(BYTE)
TARGETOBJS12= toplevel/$(TARGETNAME12).cmo

TARGETNAME13=xmlplan2plan
TARGET13=$(TARGETNAME13)$(EXE)
OPTTARGET13=$(TARGETNAME13)$(OPT)
BYTETARGET13=$(TARGETNAME13)$(BYTE)
TARGETOBJS13= toplevel/$(TARGETNAME13).cmo

TARGETNAME14=zerod
TARGET14=$(TARGETNAME14)$(EXE)
OPTTARGET14=$(TARGETNAME14)$(OPT)
BYTETARGET14=$(TARGETNAME14)$(BYTE)
TARGETOBJS14= toplevel/$(TARGETNAME14).cmo

TARGETNAME15=xqueryx2xquery
TARGET15=$(TARGETNAME15)$(EXE)
OPTTARGET15=$(TARGETNAME15)$(OPT)
BYTETARGET15=$(TARGETNAME15)$(BYTE)
TARGETOBJS15= toplevel/$(TARGETNAME15).cmo


GALAX_ALL_LNCOMMANDS=\
$(TARGETNAME1) \
$(TARGETNAME2) \
$(TARGETNAME3) \
$(TARGETNAME4) \
$(TARGETNAME5) \
$(TARGETNAME6) \
$(TARGETNAME7) \
$(TARGETNAME8) \
$(TARGETNAME9) \
$(TARGETNAME10) \
$(TARGETNAME11) \
$(TARGETNAME12) \
$(TARGETNAME13) \
$(TARGETNAME14) \
$(TARGETNAME15) 

GALAX_BYTE_COMMANDS=\
$(BYTETARGET1) \
$(BYTETARGET2) \
$(BYTETARGET3) \
$(BYTETARGET4) \
$(BYTETARGET5) \
$(BYTETARGET6) \
$(BYTETARGET7) \
$(BYTETARGET8) \
$(BYTETARGET9) \
$(BYTETARGET10) \
$(BYTETARGET11) \
$(BYTETARGET12) \
$(BYTETARGET13) \
$(BYTETARGET14) \
$(BYTETARGET15) \
ocaml-galax$(EXE)

GALAX_ALL_COMMANDS=$(GALAX_BYTE_COMMANDS)

GALAX_ALL_COMMAND_OBJS=\
$(TARGETOBJS1) \
$(TARGETOBJS2) \
$(TARGETOBJS3) \
$(TARGETOBJS4) \
$(TARGETOBJS5) \
$(TARGETOBJS6) \
$(TARGETOBJS7) \
$(TARGETOBJS8) \
$(TARGETOBJS9) \
$(TARGETOBJS10) \
$(TARGETOBJS11) \
$(TARGETOBJS12) \
$(TARGETOBJS13) \
$(TARGETOBJS14) \
$(TARGETOBJS15)

ifdef OCAMLOPT
GALAX_ALL_COMMANDS+=\
$(OPTTARGET1) \
$(OPTTARGET2) \
$(OPTTARGET3) \
$(OPTTARGET4) \
$(OPTTARGET5) \
$(OPTTARGET6) \
$(OPTTARGET7) \
$(OPTTARGET8)  \
$(OPTTARGET9) \
$(OPTTARGET10) \
$(OPTTARGET11) \
$(OPTTARGET12) \
$(OPTTARGET13) \
$(OPTTARGET14) \
$(OPTTARGET15)
endif

#########################################################################
# Section:	Main targets
# Description:
#		This is where all of the named toplevel targets are placed.
#
#	world:		Force updating of .depend files and make all
#	all:			This target builds the bytecode versions of Galax
#	install::	This target does installation for files in the main section
#	uninstall::	This target uninstalls files installed in the main section
#	clean::		This target cleans files created in the all section
#	clobber::	This target cleans files created in the all section and .depend
#	
#########################################################################

world:
	$(MAKE) all DEPDEPEND=1

byteworld:
	$(MAKE) byte DEPDEPEND=1

all: lib subcomponents lncommands

byte:	bytelib bytesubcomponents bytecommands

regression:	$(TARGET8)
	(cd regress; $(MAKE) tests)

buildbot-regression:	$(TARGET8)
	(cd regress; $(MAKE) -s buildbot-diff)

buildbot-fast-regression:	$(TARGET1)
	(cd usecases; $(MAKE) -s buildbot-diff)

install:: all $(CONF_GALAX_CONFIG) $(CONF_GALAX_MAN)
	$(CP) config/Makefile.conf $(CONF_GALAX_CONFIG)
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):" config/Makefile.galax-inst > $(CONF_GALAX_CONFIG)/Makefile.galax
	$(CP) LICENSE $(CONF_GALAX_MAN)
	$(CP) README $(CONF_GALAX_MAN)

uninstall::
	$(RM) $(CONF_GALAX_CONFIG)/Makefile.conf
	$(RM) $(CONF_GALAX_CONFIG)/Makefile.galax
	$(RM) $(CONF_GALAX_MAN)/LICENSE
	$(RM) $(CONF_GALAX_MAN)/README

realclean:
	$(MAKE) clean
	$(RM) $(LOCALPREFIX)/.depend
	$(RM) $(LOCALPREFIX)/config/Makefile.conf
	$(RM) $(LOCALPREFIX)/regress/testconfig.xml

clean::
	$(RM) $(LOCALPREFIX)/c_api/.depend
	$(RM) $(LOCALPREFIX)/tools/escaping/.depend
	$(RM) $(LOCALPREFIX)/tools/ucs2_to_utf8/.depend
	$(RM) $(LOCALPREFIX)/galapi/c_api/.depend

package-source:	clean $(CONF_PACKAGE_DIR)/$(RELEASE)
	$(RM) .depend
	$(RM) $(CONF_PACKAGE_DIR)/$(RELEASE)/galax-$(RELEASE).tar*
	cd ..; $(ZIP) $(CONF_PACKAGE_DIR)/$(RELEASE)/galax-$(RELEASE)$(ZIPEXT) $(ZIPOPT) $(notdir $(CONF_PACKAGE_SRC))

$(CONF_PACKAGE_DIR)/$(RELEASE):
	$(MKDIR) $(CONF_PACKAGE_DIR)/$(RELEASE)


#########################################################################
# Sub-Section:	Secondary targets
# Description:
#		These are the targets listed as dependencies of "all".  They each
#		have their own install, uninstall, and clean targets.
#
#		lib:		Standard Galax libraries
#		subcomponents:	Galax libraries, commands, etc. with their own builds
#		commands:	oplevel Galax commands
#########################################################################

#########################################################################
# Sub-Sub-Section:	lib
# Description:
#		These are the targets for building libraries, installing them
#		uninstalling them, and cleaning them.
#
#		lib:	Builds the library targets (static and optional shared)
#		install:Installs the library targets (static and optional shared)
#		uninstall:Uninstalls the library targets (static and optional shared)
#		clean:	Cleans up the files used to create the libraries that
#						are created by automatic rules
#########################################################################

lib: $(GALAX_LIB_TARGETS)

bytelib:	$(GALAX_LIB)

install:: lib $(CONF_GALAX_OCAMLLIB)
	for i in $(GALAX_LIB_TARGETS); do ($(CP) $$i $(CONF_GALAX_OCAMLLIB)); done
	cp META $(CONF_GALAX_OCAMLLIB)
	if test -f $(GALAX_CLIB); then ($(CP) $(GALAX_CLIB) $(CONF_GALAX_OCAMLLIB); $(RANLIB) $(CONF_GALAX_OCAMLLIB)/$(GALAX_CLIB)); fi

uninstall::
	for i in $(GALAX_LIB_TARGETS); do ($(RM) $(CONF_GALAX_OCAMLLIB)/$$i); done
	$(RM) $(CONF_GALAX_OCAMLLIB)/$(GALAX_CLIB)

clean::
	$(RM) $(GALAX_LIB) $(GALAX_OPTLIB)
	$(RM) *.o *.a */*.o */*/*.o 
	$(RM) *.cmi */*.cmi */*/*.cmi
	$(RM) *.cmo */*.cmo */*/*.cmo
	$(RM) *.cmx */*.cmx */*/*.cmx

# Individual library targets
#
$(GALAX_LIB):	$(GALAX_ALL_OBJECTS) $(GALAX_CMIFILES)
	$(OCAMLC) -a -linkall -o $(GALAX_LIB) $(GALAX_ALL_OBJECTS) $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES)

$(GALAX_OPTLIB):	$(GALAX_ALL_OPTOBJECTS) $(GALAX_CMIFILES)
	$(OCAMLOPT) -a -linkall -o $(GALAX_OPTLIB) $(GALAX_ALL_OPTOBJECTS) $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES)

#########################################################################
# Sub-Sub-Section:	subcomponents
# Description:
#		These are the targets for building any sub-component libraries
#		or applications.
#
#		subcomponents:	Builds the sub targets
#		install:	Installs the sub targets
#		uninstall:	Uninstalls the sub targets
#		clean:		Cleans up the sub targets
#########################################################################

subcomponents: lib
	for i in $(GALAX_ALL_SUBCOMPONENTS); do (if test -d $$i; then (cd $$i ; $(MAKE) all); fi); done

bytesubcomponents:	bytelib
	for i in $(GALAX_ALL_SUBCOMPONENTS); do (if test -d $$i; then (cd $$i ; $(MAKE) byte); fi); done

install:: subcomponents
	for i in $(GALAX_ALL_SUBCOMPONENTS); do (if test -d $$i; then (cd $$i ; $(MAKE) install); fi); done

uninstall::
	for i in $(GALAX_ALL_SUBCOMPONENTS); do (if test -d $$i; then (cd $$i ; $(MAKE) uninstall); fi); done

clean::
	for i in $(GALAX_ALL_SUBCOMPONENTS); do (if test -d $$i; then (cd $$i ; $(MAKE) clean); fi); done

clobber::
	for i in $(GALAX_ALL_SUBCOMPONENTS); do (if test -d $$i; then (cd $$i ; $(MAKE) clobber); fi); done

#########################################################################
# Sub-Sub-Section:	commands
# Description:
#		These are the targets for building the toplevel applications
#
#		commands:	Builds the byte and opt command targets
#		install:	Installs the command targets
#		uninstall:	Uninstalls the command targets
#		clean:		Clean is delegated to each of the actual commands
#########################################################################

commands:	lib subcomponents $(GALAX_ALL_COMMANDS)

bytecommands:	bytelib bytesubcomponents $(GALAX_BYTE_COMMANDS)

lncommands:	commands
	for i in $(GALAX_ALL_LNCOMMANDS); do ($(RM) $(LOCALPREFIX)/$${i}$(EXE)); (if test -f $(LOCALPREFIX)/$${i}$(OPT); then $(LN) $(LOCALPREFIX)/$${i}$(OPT) $(LOCALPREFIX)/$${i}$(EXE); else $(LN) $(LOCALPREFIX)/$${i}$(BYTE) $(LOCALPREFIX)/$${i}$(EXE); fi); done

clean::
	for i in $(GALAX_ALL_LNCOMMANDS); do ($(RM) $(LOCALPREFIX)/$${i}$(EXE)); done

install::	commands $(CONF_GALAX_BIN)
	for i in $(GALAX_ALL_COMMANDS); do ($(CP) $$i $(CONF_GALAX_BIN)); done
	for i in $(GALAX_ALL_LNCOMMANDS); do ($(RM) $(CONF_GALAX_BIN)/$${i}$(EXE)); (if test -f $(CONF_GALAX_BIN)/$${i}$(OPT); then $(LN) $(CONF_GALAX_BIN)/$${i}$(OPT) $(CONF_GALAX_BIN)/$${i}$(EXE); else $(LN) $(CONF_GALAX_BIN)/$${i}$(BYTE) $(CONF_GALAX_BIN)/$${i}$(EXE); fi); done

uninstall::
	for i in $(GALAX_ALL_COMMANDS); do ($(RM) $(CONF_GALAX_BIN)/$$i); done
	for i in $(GALAX_ALL_LNCOMMANDS); do ($(RM) $(CONF_GALAX_BIN)/$${i}$(EXE)); done

clean::
	$(RM) $(GALAX_ALL_COMMANDS) $(GALAX_ALL_COMMAND_OBJS) $(GALAX_ALL_COMMAND_OBJS:.cmo=.cmx)

# Individual command targets
#
$(BYTETARGET1):	$(GALAX_LIB) $(TARGETOBJS1)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS1)

$(BYTETARGET2):	$(GALAX_LIB) $(TARGETOBJS2)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS2)

$(BYTETARGET3):	$(GALAX_LIB) $(TARGETOBJS3)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS3)

$(BYTETARGET4):	$(GALAX_LIB) $(TARGETOBJS4)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS4)

$(BYTETARGET5):	$(GALAX_LIB) $(TARGETOBJS5)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS5)

$(BYTETARGET6):	$(GALAX_LIB) $(TARGETOBJS6)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS6)

$(BYTETARGET7):	$(GALAX_LIB) $(TARGETOBJS7)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS7)

$(BYTETARGET8):	$(GALAX_LIB) $(TARGETOBJS8)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(GALAX_TESTING) $(TARGETOBJS8)

toplevel/$(TARGETNAME9).cmo: toplevel/$(TARGETNAME9).ml
	$(OCAMLC) -thread $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) -c $<

# YUCK: Webgui links in Galax.cmxa just for pretty printing
$(BYTETARGET9): $(GALAX_LIB) $(TARGETOBJS9)
	$(OCAMLC) -thread -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAXD_ALL_LIBS) $(TARGETOBJS9)
#	$(OCAMLC) -thread -custom -linkall -o $@ $(OCAMLC_FLAGS) $(OCAML_LIB_INCLUDES) pcre.cma unix.cma netstring.cma str.cma threads.cma $(TARGETOBJS9)

toplevel/$(TARGETNAME10).cmo: toplevel/$(TARGETNAME10).ml
	$(OCAMLC) -thread $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) -c $<

$(BYTETARGET10): $(GALAX_LIB) $(TARGETOBJS10) 
	$(OCAMLC) -thread -custom -linkall -o $(BYTETARGET10) $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAXD_ALL_LIBS) $(TARGETOBJS10)

$(BYTETARGET11): $(GALAX_LIB) $(TARGETOBJS11)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS11)

$(BYTETARGET12): $(GALAX_LIB) $(TARGETOBJS12)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS12)

$(BYTETARGET13): $(GALAX_LIB) $(TARGETOBJS13)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS13)

toplevel/$(TARGETNAME14).cmo: toplevel/$(TARGETNAME14).ml
	$(OCAMLC) -thread $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) -c $<

$(BYTETARGET14): $(GALAX_LIB) $(TARGETOBJS14)
	$(OCAMLC) -thread -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAXD_ALL_LIBS) $(TARGETOBJS14)

$(BYTETARGET15): $(GALAX_LIB) $(TARGETOBJS15)
	$(OCAMLC) -thread -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAXD_ALL_LIBS) $(TARGETOBJS15)

$(OPTTARGET1):	$(GALAX_OPTLIB) $(TARGETOBJS1:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS1:.cmo=.cmx)

$(OPTTARGET2):	$(GALAX_OPTLIB) $(TARGETOBJS2:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS2:.cmo=.cmx)

$(OPTTARGET3):	$(GALAX_OPTLIB) $(TARGETOBJS3:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS3:.cmo=.cmx)

$(OPTTARGET4):	$(GALAX_OPTLIB) $(TARGETOBJS4:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS4:.cmo=.cmx)

$(OPTTARGET5):	$(GALAX_OPTLIB) $(TARGETOBJS5:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS5:.cmo=.cmx)

$(OPTTARGET6):	$(GALAX_OPTLIB) $(TARGETOBJS6:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS6:.cmo=.cmx)

$(OPTTARGET7):	$(GALAX_OPTLIB) $(TARGETOBJS7:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS7:.cmo=.cmx)

$(OPTTARGET8):	$(GALAX_OPTLIB) $(TARGETOBJS8:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(GALAX_TESTING_OPTOBJECTS) $(TARGETOBJS8:.cmo=.cmx)

toplevel/$(TARGETNAME9).cmx: toplevel/$(TARGETNAME9).ml
	$(OCAMLOPT) -thread $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) -c $<

$(OPTTARGET9): $(GALAX_OPTLIB) $(TARGETOBJS9:.cmo=.cmx)
	$(OCAMLOPT) -thread -linkall -o $(OPTTARGET9) $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAXD_ALL_OPTLIBS) $(TARGETOBJS9:.cmo=.cmx)
#	$(OCAMLOPT) -thread -linkall -o $@ $(OCAMLOPT_FLAGS) $(OCAML_LIB_INCLUDES) pcre.cmxa unix.cmxa netstring.cmxa str.cmxa threads.cmxa $(TARGETOBJS9:.cmo=.cmx)

toplevel/$(TARGETNAME10).cmx: toplevel/$(TARGETNAME10).ml
	$(OCAMLOPT) -thread $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) -c $<

$(OPTTARGET10):	$(GALAX_OPTLIB) $(TARGETOBJS10:.cmo=.cmx)
	$(OCAMLOPT) -thread -linkall -o $(OPTTARGET10) $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAXD_ALL_OPTLIBS) $(TARGETOBJS10:.cmo=.cmx)

$(OPTTARGET11):	$(GALAX_OPTLIB) $(TARGETOBJS11:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS11:.cmo=.cmx)

$(OPTTARGET12):	$(GALAX_OPTLIB) $(TARGETOBJS12:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS12:.cmo=.cmx)

$(OPTTARGET13):	$(GALAX_OPTLIB) $(TARGETOBJS13:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS13:.cmo=.cmx)

toplevel/$(TARGETNAME14).cmx: toplevel/$(TARGETNAME14).ml
	$(OCAMLOPT) -thread $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) -c $<

$(OPTTARGET14):	$(GALAX_OPTLIB) $(TARGETOBJS14:.cmo=.cmx)
	$(OCAMLOPT) -thread -linkall -o $(OPTTARGET14) $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAXD_ALL_OPTLIBS) $(TARGETOBJS14:.cmo=.cmx)

$(OPTTARGET15):	$(GALAX_OPTLIB) $(TARGETOBJS15:.cmo=.cmx)
	$(OCAMLOPT) -thread -linkall -o $(OPTTARGET15) $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAXD_ALL_OPTLIBS) $(TARGETOBJS15:.cmo=.cmx)

ocaml-galax$(EXE):	$(GALAX_LIB)
	$(OCAMLMKTOP) -custom -linkall -o $@ $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS)

#########################################################################
# Sub-Sub-Section:	Source files
# Description:
#		These are the targets for creating source files
#
#		base/pervasive.xq: 			required to build
#						base/pervasive.ml and for install
#		base/pervasive.ml:			required for libraries
#		base/conf.ml:				required for libraries
#		namespace/qname_lexer_utf8.ml:		required for UTF8 libraries
#		lexer/opening_tag_lexer_utf8.ml:	required for UTF8 libraries
#		lexer/closing_tag_lexer_utf8.ml:	required for UTF8 libraries
#		lexer/text_tag_lexer_utf8.ml:		required for UTF8 libraries
#		namespace/qname_lexer_iso88591.ml:	required for ISO88591 libraries
#		lexer/opening_tag_lexer_iso88591.ml:	required for ISO88591 libraries
#		lexer/closing_tag_lexer_iso88591.ml:	required for ISO88591 libraries
#		lexer/text_tag_lexer_iso88591.ml:	required for ISO88591 libraries
#		schema/schema_import.ml:		required for libraries
#		parsing/parse_query.ml[i]:		required for libraries
#               toplevel/top_server.ml:                 required for threaded/non-threaded server
#
#		clean::		removes created source files, including mll-generated
#				ml files
#		install::	installs base/pervasive.xq
#		uninstall::	removes base/pervasive.xq
#########################################################################

# Source code generation
#
base/pervasive.xq:	stdlib/pervasive.xq
	$(CP) stdlib/pervasive.xq base/pervasive.xq

base/pervasive.ml:	tools/escaping/escaping$(EXE) base/pervasive.xq
	tools/escaping/escaping$(EXE) base/pervasive.xq

base/conf.ml: base/conf.mlp
	@$(RM) base/conf.ml 
	$(SED) -e 's|%%RELEASE%%|$(RELEASE)|' \
	    -e 's|%%STATUS%%|$(STATUS)|' \
	    -e 's|%%LIBDIR%%|$(CONF_GALAX_LIB)|' \
	    -e 's|%%XMLURI%%|$(XMLURI)|' \
	    -e 's|%%XMLNSURI%%|$(XMLNSURI)|' \
	    -e 's|%%XSURI%%|$(XSURI)|' \
	    -e 's|%%XSDURI%%|$(XSDURI)|' \
	    -e 's|%%XSIURI%%|$(XSIURI)|' \
	    -e 's|%%FNURI%%|$(FNURI)|' \
	    -e 's|%%XQUERYXURI%%|$(XQUERYXURI)|' \
	    -e 's|%%OPURI%%|$(OPURI)|' \
	    -e 's|%%FSURI%%|$(FSURI)|' \
	    -e 's|%%COLLURI%%|$(COLLURI)|' \
	    -e 's|%%ERRURI%%|$(XQTURI)|' \
	    -e 's|%%LOCALURI%%|$(LOCALURI)|' \
	    -e 's|%%GLXURI%%|$(GLXURI)|' \
            base/conf.mlp > base/conf.ml

base/galax_camomile.ml: base/galax_camomile_$(CONF_CAMOMILEVERSION).ml
	$(CP) base/galax_camomile_$(CONF_CAMOMILEVERSION).ml base/galax_camomile.ml

namespace/qname_lexer_utf8.mll:	tools/insert_variant namespace/pxp_lex_defs_utf8.def namespace/qname_lexer.src
	cd namespace; $(OCAML) ../tools/insert_variant -variant utf8 qname_lexer.src

lexing/opening_tag_lexer_utf8.mll:	lexing/pxp_lex_defs_utf8.def tools/insert_variant lexing/opening_tag_lexer.src
	cd lexing ; $(OCAML) ../tools/insert_variant -variant utf8 opening_tag_lexer.src

lexing/closing_tag_lexer_utf8.mll:	lexing/pxp_lex_defs_utf8.def tools/insert_variant lexing/closing_tag_lexer.src
	cd lexing ; $(OCAML) ../tools/insert_variant -variant utf8 closing_tag_lexer.src

lexing/text_lexer_utf8.mll:	lexing/pxp_lex_defs_utf8.def tools/insert_variant lexing/text_lexer.src
	cd lexing ; $(OCAML) ../tools/insert_variant -variant utf8 text_lexer.src

namespace/qname_lexer_iso88591.mll:	tools/insert_variant namespace/pxp_lex_defs_iso88591.def namespace/qname_lexer.src
	cd namespace; $(OCAML) ../tools/insert_variant -variant iso88591 qname_lexer.src

lexing/opening_tag_lexer_iso88591.mll:	lexing/pxp_lex_defs_iso88591.def tools/insert_variant lexing/opening_tag_lexer.src
	cd lexing ; $(OCAML) ../tools/insert_variant -variant iso88591 opening_tag_lexer.src

lexing/closing_tag_lexer_iso88591.mll:	lexing/pxp_lex_defs_iso88591.def tools/insert_variant lexing/closing_tag_lexer.src
	cd lexing ; $(OCAML) ../tools/insert_variant -variant iso88591 closing_tag_lexer.src

lexing/text_lexer_iso88591.mll:	lexing/pxp_lex_defs_iso88591.def tools/insert_variant lexing/text_lexer.src
	cd lexing ; $(OCAML) ../tools/insert_variant -variant iso88591 text_lexer.src

schema/schema_import.ml: schema/schema_import.mlp
	$(CAMLP4O) -printer OCaml -impl schema/schema_import.mlp > $@

wsdl/wsdl_load.ml: wsdl/wsdl_load.mlp
	$(CAMLP4O) -printer OCaml -impl wsdl/wsdl_load.mlp > $@

parsing/parse_xquery.ml parsing/parse_xquery.mli:	parsing/parse_xquery.mly
	$(OCAMLYACC) -v parsing/parse_xquery.mly

install::	base/pervasive.xq  $(CONF_GALAX_LIB)
	$(CP) base/pervasive.xq $(CONF_GALAX_LIB)

uninstall::
	$(RM) $(CONF_GALAX_LIB)/pervasive.xq

clean::
	$(RM) $(GALAX_MLLFILES:.mll=.ml)
	$(RM) $(GALAX_CMIFILES)
	$(RM) base/pervasive.xq
	$(RM) base/pervasive.ml
	$(RM) base/conf.ml
	$(RM) base/galax_camomile.ml
	$(RM) namespace/qname_lexer_utf8.mll
	$(RM) lexing/opening_tag_lexer_utf8.mll
	$(RM) lexing/closing_tag_lexer_utf8.mll
	$(RM) lexing/text_lexer_utf8.mll
	$(RM) namespace/qname_lexer_iso88591.mll
	$(RM) lexing/opening_tag_lexer_iso88591.mll
	$(RM) lexing/closing_tag_lexer_iso88591.mll
	$(RM) lexing/text_lexer_iso88591.mll
	$(RM) schema/schema_import.ml
	$(RM) toplevel/top_server.ml
	$(RM) parsing/parse_xquery.ml parsing/parse_xquery.mli parsing/parse_xquery.output
	$(RM) wsdl/wsdl_load.ml

#########################################################################
# Sub-Sub-Sub-Section:	Source files
# Description:
#		These are the targets for creating source files dependents
#
#	namespace/pxp_lex_defs_utf8.def:required for namespace/qname_lexer_utf8.ml
#	lexing/pxp_lex_defs_utf8.def:	required for lexing/*_utf8.ml
#	tools/charsets/pxp_lex_defs_utf8.def:required for lexing/pxp_lex_defs_utf8.def
#		clean::		removes created source files
#		install::	installs base/pervasive.xq
#		uninstall::	removes base/pervasive.xq
#########################################################################

namespace/pxp_lex_defs_utf8.def:	tools/charsets/pxp_lex_defs_utf8.def
	cd namespace; $(LN) ../tools/charsets/pxp_lex_defs_utf8.def pxp_lex_defs_utf8.def

lexing/pxp_lex_defs_utf8.def:	tools/charsets/pxp_lex_defs_utf8.def
	cd lexing ; $(LN) ../tools/charsets/pxp_lex_defs_utf8.def pxp_lex_defs_utf8.def

tools/charsets/pxp_lex_defs_utf8.def:	tools/ucs2_to_utf8/ucs2_to_utf8$(EXE) tools/charsets/pxp_lex_defs_generic.def tools/charsets/pxp_lex_defs_drv_utf8.def
	tools/ucs2_to_utf8/ucs2_to_utf8$(EXE) tools/charsets/pxp_lex_defs_generic.def $@
	$(CAT) tools/charsets/pxp_lex_defs_drv_utf8.def >> $@

lexing/pxp_lex_defs_iso88591.def:	tools/charsets/pxp_lex_defs_iso88591.def
	cd lexing ; $(LN) ../tools/charsets/pxp_lex_defs_iso88591.def pxp_lex_defs_iso88591.def

clean::
	$(RM) namespace/pxp_lex_defs_utf8.def
	$(RM) lexing/pxp_lex_defs_utf8.def
	$(RM) tools/charsets/pxp_lex_defs_utf8.def
	$(RM) lexing/pxp_lex_defs_iso88591.def

#########################################################################
# Sub-Sub-Sub-Section:	Tools
# Description:
#		These are the targets for creating tools that generate source code
#
#		tools/escaping/escaping:		character escaping tool
#		tools/ucs2_to_utf/ucs2_to_utf8:		UTF8 character conversion
#		clean::					removes created source files
#		install::				installs base/pervasive.xq
#		uninstall::				removes base/pervasive.xq
#########################################################################

# Tools
#
tools/escaping/escaping$(EXE):
	cd tools/escaping ; $(MAKE) escaping$(EXE)

tools/ucs2_to_utf8/ucs2_to_utf8$(EXE):
	cd tools/ucs2_to_utf8 ; $(MAKE) ucs2_to_utf8$(EXE)

install::

uninstall::

clean::
	cd tools/escaping ; $(MAKE) clean
	cd tools/ucs2_to_utf8 ; $(MAKE) clean

#########################################################################
# Section:	Makefile post-includes
# Description:
#		This is where the dependency file is included and generated
#		We place the dependencies to make sure that various files
#		are generated before we need them.
#########################################################################

clobber::
	$(RM) .depend

updatedepend::
	$(MAKE) .depend DEPDEPEND=1
	for i in $(GALAX_ALL_SUBCOMPONENTS) tools/ucs2_to_utf8; do (cd $$i ; $(MAKE) updatedepend); done

ifdef DEPDEPEND
.depend: $(GALAX_MLIFILES) $(GALAX_MLFILES) $(GALAX_ALL_COMMAND_OBJS:.cmo=.ml)
	$(OCAMLDEP) $(GALAX_TOOLS_INCLUDES) $(GALAX_INCLUDES) $(GALAX_MLIFILES) $(GALAX_MLFILES) $(GALAX_ALL_COMMAND_OBJS:.cmo=.ml) | $(SED) -e 's?\([a-z]\)\\?\1\/?g' > .depend
else
.depend:
	$(OCAMLDEP) $(GALAX_TOOLS_INCLUDES) $(GALAX_INCLUDES) $(GALAX_MLIFILES) $(GALAX_MLFILES) $(GALAX_ALL_COMMAND_OBJS:.cmo=.ml) | $(SED) -e 's?\([a-z]\)\\?\1\/?g' > .depend
endif

ifndef NODEPEND
include .depend
endif


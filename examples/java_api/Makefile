#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

#########################################################################
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax Java API examples
# History:
#	$Log: Makefile,v $
#	Revision 1.38  2007/12/06 02:19:14  mff
#	
#	December 5, 2007 - Mary
#	
#	  o Java API
#	    - Changes to get Java API to link
#	
#	    TODO: align examples/galapi/java_api
#	
#	Revision 1.37  2007/02/01 22:08:46  simeon
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
#	Revision 1.36  2006/05/17 15:27:39  mff
#	May 17, 2006 - Mary
#	
#	  o Browser interface/Website
#	    - Updated to align w/ GODI configuration & Galax 0.6.5.
#	    - Fixed CGI printing bug.  All output now printed correctly.
#	
#	  o Documentation
#	    - Updating documentation for GODI release.
#	
#	  o Logical Plan Printing
#	    - Fixed old bug that replicated algebra: namespace.
#	
#	  o Loading
#	    - Made interface names more mneumonic
#	
#	  o Java API & Examples Makefiles
#	    - Added path to CAPI libraries
#	    - Fix to install: target
#	
#	  TODO: Document command-line options :=(
#	
#	Revision 1.35  2006/05/15 15:20:36  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.34  2006/05/12 18:15:00  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.33  2006/04/17 18:10:40  car
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
# Section:	Local variables
# Description:
#		These variables are used for targets of this Makefile
###########################################################################

EXAMPLE=example
TEST=test

JAVACLASSPATH=-classpath .:$(LOCALPREFIX)/galapi/java_api/$(GALAX_JAVAAPI_ARCHIVE)
API=java_api

EXFILES=\
Example.java \
Test.java \
example.expect \
test.expect

#########################################################################
# Sub-Section:	Targets
# Description:
#		These are the configuration-dependent targets built by "all"
###########################################################################
ALL_PROGRAMS=runexample runtest

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

tests:	$(ALL_PROGRAMS)

all:	$(ALL_PROGRAMS)

byte:

install:	$(CONF_GALAX_EXAMPLES)/java_api
	for i in $(EXFILES); do ($(CP) $$i $(CONF_GALAX_EXAMPLES)/java_api); done
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):g" Makefile-inst > $(CONF_GALAX_EXAMPLES)/java_api/Makefile

uninstall:
	for i in $(EXFILES); do ($(RM) $(CONF_GALAX_EXAMPLES)/java_api/$$i); done
	$(RM) $(CONF_GALAX_EXAMPLES)/java_api/Makefile

clean:
	$(RM) Example.class Test.class example.xml test.xml

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

runexample: Example.class
	(export $(LD_LIBRARY_PATH_VAR)=$(LD_LIBRARY_PATH):$(LOCALPREFIX)/galapi/c_api:$(LOCALPREFIX)/galapi/java_api:$(CONF_C_LIBPCRE):$(CONF_C_LIBBDB); \
        $(JAVA) -classpath .:$(LOCALPREFIX)/galapi/java_api/galapi.jar Example >$(EXAMPLE).out; )
	if [[ -z "`cmp $(EXAMPLE).out $(EXAMPLE).expect`" ]]; then echo "$(EXAMPLE) passed"; else $(MAKE) TEST=$(EXAMPLE) error; fi;

runtest: Test.class 
	(export $(LD_LIBRARY_PATH_VAR)=$(LD_LIBRARY_PATH):$(LOCALPREFIX)/galapi/c_api:$(LOCALPREFIX)/galapi/java_api:$(CONF_C_LIBPCRE):$(CONF_C_LIBBDB); \
	$(JAVA) $(JAVACLASSPATH) Test >$(TEST).out)
	if [[ -z "`cmp $(TEST).out $(TEST).expect`" ]]; then echo "$(TEST) passed"; else $(MAKE) TEST=$(TEST) error; fi;

$(CONF_GALAX_EXAMPLES)/java_api:
	$(MKDIR) $@

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.42 2007/07/05 08:35:54 simeon Exp $ #

#########################################################################
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax C API examples
# History:
#	$Log: Makefile,v $
#	Revision 1.42  2007/07/05 08:35:54  simeon
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
#	Revision 1.41  2007/02/01 22:08:46  simeon
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
#	Revision 1.40  2006/05/15 15:20:36  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.39  2006/05/12 18:15:00  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.38  2006/04/17 18:10:40  car
#	April 17, 2006 - Chris Rath
#	  o Changes necessary to align with GODI
#	
#########################################################################

#########################################################################
# Section:	Default target
# Description:
#		The default target for this Makefile is "all"
#########################################################################
default: all

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
EXAMPLEOBJECT=$(EXAMPLE).o

TEST1=test
TEST1OBJECT=$(TEST1).o

#########################################################################
# Sub-Section:	C compilation variables and rules
# Description:
#		These are the variables and rules for building C object files
###########################################################################
.c.o:
	$(CC) $(CFLAGS) $(CCFLAGS) $(GALAX_CAPI_INCLUDES) -c $<

#########################################################################
# Sub-Section:	Targets
# Description:
#		These are the configuration-dependent targets built by "all"
###########################################################################
ALL_PROGRAMS=$(EXAMPLE)$(EXE) $(TEST1)$(EXE)
ALL_OBJECTS=$(EXAMPLEOBJECT) $(TEST1OBJECT)
ALL_SOURCE=$(ALL_OBJECTS:.o=.c)
ALL_EXPECT=$(ALL_OBJECTS:.o=.expect)
ALL_TESTS=run$(EXAMPLE) run$(TEST1)

ifeq ($(SUPPORTS_SHARED_LIBRARIES), true)
LDFLAGS=$(GALAX_ALL_CAPI_OPTSHAREDLIBS)
else
LDFLAGS=$(GALAX_ALL_CAPI_OPTLIBS)
endif

#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the ALL_PROGRAMS
#  install::   This target does installation for C API example files
#  uninstall:: This target uninstalls C API example files
#  clean::     This target cleans files created in the all section
#########################################################################

all:

#all: $(ALL_PROGRAMS)

byte:

tests:	$(ALL_TESTS)

install: $(CONF_GALAX_EXAMPLES)/c_api
	for i in $(ALL_SOURCE) $(ALL_EXPECT); do ($(CP) $$i $(CONF_GALAX_EXAMPLES)/c_api); done
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):g" Makefile-inst > $(CONF_GALAX_EXAMPLES)/c_api/Makefile

uninstall:
	for i in $(ALL_SOURCE) $(ALL_EXPECT); do ($(RM) $(CONF_GALAX_EXAMPLES)/c_api/$$i); done
	$(RM) $(CONF_GALAX_EXAMPLES)/c_api/Makefile

clean:
	$(RM) $(ALL_PROGRAMS)
	$(RM) *.cm[oixa] *.[oa] *.class *~
	$(RM) to_html.xml
	$(RM) *.xml *.cmp

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

$(CONF_GALAX_EXAMPLES)/c_api:
	$(MKDIR) $(CONF_GALAX_EXAMPLES)/c_api

$(EXAMPLE)$(EXE): $(EXAMPLEOBJECT)
	$(CC) $(EXAMPLEOBJECT) $(LDFLAGS) -o $@

$(TEST1)$(EXE): $(TEST1OBJECT)
	$(CC) $(TEST1OBJECT) $(LDFLAGS) -g -o $@

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "tests"
#########################################################################

run$(EXAMPLE): $(EXAMPLE).out $(EXAMPLE).expect
	if [[ -z "`cmp $(EXAMPLE).out $(EXAMPLE).expect`" ]]; then echo "$(EXAMPLE) passed"; else $(MAKE) TEST=$(EXAMPLE) error; fi;

$(EXAMPLE).out:	$(EXAMPLE)$(EXE)
	./$(EXAMPLE)$(EXE) > $@

run$(TEST1): $(TEST1).out $(TEST1).expect
	if [[ -z "`cmp $(TEST1).out $(TEST1).expect`" ]]; then echo "$(TEST1) passed"; else $(MAKE) TEST=$(TEST1) error; fi;

$(TEST1).out:	$(TEST1)$(EXE)
	./$(TEST1)$(EXE) > $@

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


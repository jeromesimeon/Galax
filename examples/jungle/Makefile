#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.8 2007/02/01 22:08:46 simeon Exp $ #

#########################################################################
# Author:	Christopher A. Rath (AT&T Labs Research)
# Descripton:
#		This is a replacement Makefile for the Galax XXX
# History:
#	$Log: Makefile,v $
#	Revision 1.8  2007/02/01 22:08:46  simeon
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
#	Revision 1.7  2006/05/15 15:20:36  car
#	May 15, 2006 - Chris Rath
#	  o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#	    - Only creates the byte-code galax library and byte-code toplevel applications.
#	  o Added a new target "byteworld" that works like "make world"
#		 - Only creates the byte-code galax library and byte-code toplevel applications.
#	
#	Revision 1.6  2006/05/12 18:15:00  car
#	May 12, 2006 - Chris Rath
#	  o Fixed missing files and typos in Makefile.galax
#	  o Added "regression" target to top level Makefile.
#	  o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#	  o Updated all lower-level Makefiles to ensure they all have the standard targets
#	
#	Revision 1.5  2006/04/17 18:10:40  car
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

EXAMPLE=ml-jungle

ALL_TARGETS=$(EXAMPLE)$(OPT)
ALL_SOURCES=$(EXAMPLE).ml
ALL_TESTS=runcmd runcaml

CEXAMPLE=c-jungle
ifeq ($(CONF_WITH_API_C), true)
ALL_TARGETS+=$(CEXAMPLE)$(EXE)
ALL_SOURCES+=$(CEXAMPLE).c
ALL_TESTS+=runc
endif

JEXAMPLE=Jungle
ifeq ($(CONF_WITH_API_JAVA), true)
JAVACLASSPATH=-classpath .:$(LOCALPREFIX)/galapi/java_api/$(GALAX_JAVAAPI_ARCHIVE)
ALL_TARGETS+=$(JEXAMPLE).class
ALL_SOURCES+=$(JEXAMPLE).java
ALL_TESTS+=runjava
endif

ifeq ($(SUPPORTS_SHARED_LIBRARIES), true)
LDFLAGS=$(GALAX_ALL_CAPI_OPTSHAREDLIBS)
else
LDFLAGS=$(GALAX_ALL_CAPI_OPTLIBS)
endif

.PHONY: tmp/XMark-main.db 

#########################################################################
# Sub-Section: C compilation variables and rules
# Description:
#     These are the variables and rules for building C object files
###########################################################################
.c.o:
	$(CC) $(CFLAGS) $(CCFLAGS) $(GALAX_CAPI_INCLUDES) -c $<

#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the jungle C API tests
#  install::   This target does installation for jungle C API tests
#  uninstall:: This target uninstalls jungle C API tests
#  clean::     This target cleans files created in the all section
#########################################################################

all:	$(ALL_TARGETS)

byte:

install:: all $(CONF_GALAX_EXAMPLES)/jungle
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):" Makefile-inst > $(CONF_GALAX_EXAMPLES)/jungle/Makefile
	for i in $(ALL_SOURCES); do ($(CP) $$i $(CONF_GALAX_EXAMPLES)/jungle); done
	$(CP) jungle1.xq $(CONF_GALAX_EXAMPLES)/jungle
	$(CP) jungle.expect $(CONF_GALAX_EXAMPLES)/jungle

uninstall::
	$(RM) $(CONF_GALAX_EXAMPLES)/jungle/Makefile
	for i in $(ALL_SOURCES); do ($(RM) $(CONF_GALAX_EXAMPLES)/jungle/$$i); done
	$(RM) $(CONF_GALAX_EXAMPLES)/jungle/jungle1.xq
	$(RM) $(CONF_GALAX_EXAMPLES)/jungle/jungle.expect

clean::
	$(RM) $(ALL_TARGETS)
	$(RM) *.o *.cmo *.cmx *.cmi *.out
	if test -d tmp; then $(DELTREE) tmp; else :; fi

clobber::

#########################################################################
# Sub-Section:	Targets
# Description:
#		These are the configuration-dependent targets built by "all"
###########################################################################

tests: $(ALL_TESTS)

load: tmp tmp/XMark-Metadata.db

tmp:
	$(MKDIR) tmp

tmp/XMark-Metadata.db:
	$(LOCALPREFIX)/tools/Jungle/shredded-jungle-load.opt -store_dir tmp -store_name XMark $(LOCALPREFIX)/usecases/docs/xmark.xml

runcmd:	load
	($(RM) cmd-jungle.out; \
	export LD_LIBRARY_PATH=$(CONF_C_LIBBDB):$(LOCALPREFIX)/galapi/c_api; \
	$(LOCALPREFIX)/tools/Jungle/glx$(OPT) -serialize wf jungle1.xq > cmd-jungle.out)
	if [[ -z "`cmp cmd-jungle.out jungle.expect`" ]]; then echo "examples/jungle/commend-line query passed"; else $(MAKE) TEST=cmd-jungle error; fi; 

runcaml: $(EXAMPLE)$(OPT) load
	($(RM) $(EXAMPLE).out  \
	export LD_LIBRARY_PATH=$(CONF_C_LIBBDB):$(LOCALPREFIX)/galapi/c_api; \
	if test -f $(EXAMPLE)(OPT); then ./$(EXAMPLE)$(OPT) > $(EXAMPLE).out; fi ;)
	if [[ -z "`cmp ml-jungle.out jungle.expect`" ]]; then echo "examples/jungle/ml-jungle passed"; else $(MAKE) TEST=ml-jungle error; fi; 

runjava: Jungle.class load
	(export $(LD_LIBRARY_PATH_VAR)=$(LD_LIBRARY_PATH):$(LOCALPREFIX)/galapi/c_api:$(LOCALPREFIX)/galapi/java_api:$(CONF_C_LIBBDB); \
        $(JAVA) $(JAVACLASSPATH) Jungle >java-jungle.out; )
	if [[ -z "`cmp java-jungle.out jungle.expect`" ]]; then echo "examples/jungle/java-jungle passed"; else $(MAKE) TEST=java-jungle error; fi; 

runc: $(CEXAMPLE)$(EXE) load
	($(RM) $(CEXAMPLE).out \
	export LD_LIBRARY_PATH=$(CONF_C_LIBBDB):$(LOCALPREFIX)/galapi/c_api; \
	if test -f $(CEXAMPLE)$(EXE); then ./$(CEXAMPLE)$(EXE) >$(CEXAMPLE).out; fi; )
	if [[ -z "`cmp c-jungle.out jungle.expect`" ]]; then echo "examples/jungle/c-jungle passed"; else $(MAKE) TEST=c-jungle error; fi; 

$(EXAMPLE)$(OPT): ml-jungle.cmx
	$(OCAMLOPT) $(OPTOCAMLFLAGS) -o $@ $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) ml-jungle.cmx

$(CEXAMPLE)$(EXE): c-jungle.o
	$(CC) c-jungle.o $(LDFLAGS) -o $@

$(CONF_GALAX_EXAMPLES)/jungle:
	$(MKDIR) $(CONF_GALAX_EXAMPLES)/jungle

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


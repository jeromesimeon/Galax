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
# Author:   Christopher A. Rath (AT&T Labs Research)
# Descripton:
#     This is a replacement Makefile for the Galax Java API.
# History:
#  $Log: Makefile,v $
#  Revision 1.25  2007/05/02 19:31:00  mff
#  May 2, 2007 - Mary
#
#    Changes to prepare for separate compilation of modules:
#
#    o Processing_context:
#      - processing_context now contains STATIC context for
#        PROGRAM unit, i.e., configuration options and values.
#
#      - module_processing_context contains STATIC context for
#        MODULE unit & is contained in norm_context.
#        name_generators moved here from processing_context.
#
#    o Algebra_context:
#      - algebra_context contains DYNAMIC context for PROGRAM unit.
#        alive_documents moved here from processing_context.
#
#  Revision 1.24  2007/02/01 22:08:47  simeon
#  February 1, 2007 - Jerome
#
#    o Code cleanup:
#       - Cleaned up all the source file headers. Added module
#         descriptions when missing, as well as CVS Id.
#       - Removed obsolete modules: Optimization_rules_treepattern_old,
#         Factorize_sideeffects.
#
#    o AST Walker:
#       - Added support for a generic fold operation on the AST (useful
#         to compute a boolean property on the AST for instance).
#
#    o Normalization:
#       - Small fix to the normalization of comparisons to re-enable join
#         detection, not using let bindings for the comparator
#         anymore. [hack]
#
#    o Rewriting:
#       - Added a judgment to check for side-effects, removed
#         corresponding obsolete judgment in Factorization.
#       - Moved the snap removal rule from optimization to rewriting,
#         cleaning up the plans as early as possible.
#
#  Revision 1.23  2006/05/17 15:27:39  mff
#  May 17, 2006 - Mary
#
#    o Browser interface/Website
#      - Updated to align w/ GODI configuration & Galax 0.6.5.
#      - Fixed CGI printing bug.  All output now printed correctly.
#
#    o Documentation
#      - Updating documentation for GODI release.
#
#    o Logical Plan Printing
#      - Fixed old bug that replicated algebra: namespace.
#
#    o Loading
#      - Made interface names more mneumonic
#
#    o Java API & Examples Makefiles
#      - Added path to CAPI libraries
#      - Fix to install: target
#
#    TODO: Document command-line options :=(
#
#  Revision 1.22  2006/05/15 15:20:37  car
#  May 15, 2006 - Chris Rath
#    o Added a new target, "byte" to the toplevel and subordinate Makefiles.
#      - Only creates the byte-code galax library and byte-code toplevel applications.
#    o Added a new target "byteworld" that works like "make world"
#  	 - Only creates the byte-code galax library and byte-code toplevel applications.
#
#  Revision 1.21  2006/05/12 18:15:01  car
#  May 12, 2006 - Chris Rath
#    o Fixed missing files and typos in Makefile.galax
#    o Added "regression" target to top level Makefile.
#    o Removed regress/testconfig.xml from CVS; this file is now generated from testconfig-tmpl.xml
#    o Updated all lower-level Makefiles to ensure they all have the standard targets
#
#  Revision 1.20  2006/04/17 18:10:40  car
#  April 17, 2006 - Chris Rath
#    o Changes necessary to align with GODI
#
#########################################################################

#########################################################################
# Section:  Default target
# Description:
#     The default target for this Makefile is "all"
#########################################################################

default:	all

#########################################################################
# Section:  Makefile pre-includes
# Description:
#     This is where the file(s) generated during by the Configure script
#     are included.  If config/Makefile.conf does not exist, the
#     make will fail.
#
#     Makefile.galax:   Variables defined for compiling and linking Galax
#                       applications in the build environment
#########################################################################
LOCALPREFIX=../..
include $(LOCALPREFIX)/config/Makefile.galax

#########################################################################
# Section:  Local variables
# Description:
#     These variables are used for targets of this Makefile
###########################################################################

DOCDIR=doc

#########################################################################
# Sub-Section: C Objects
# Description:
#     These are the C object files that go into the Java JNI library
#		Note: These object files are imported from the C API directory except
#		for galax_jni_stub.o
###########################################################################
GALAX_JAVAAPI_OPTCOBJECTS=galax_jni_stub.o ../c_api/galax_wrap_opt.o ../c_api/galax_util.o \
            ../c_api/galax_stub.o ../c_api/itemlist.o 

#########################################################################
# Sub-Section: Java Sources
# Description:
#     These are the C object files that go into the Java JNI library
#     Note: These object files are imported from the C API directory except
#     for galax_jni_stub.o
###########################################################################

GALAX_JAVAAPI_JAVASOURCES=$(GALAX_JAVA_PACKAGE)/Atomic.java $(GALAX_JAVA_PACKAGE)/AtomicList.java \
            $(GALAX_JAVA_PACKAGE)/Attribute.java $(GALAX_JAVA_PACKAGE)/AttributeList.java \
            $(GALAX_JAVA_PACKAGE)/Comment.java $(GALAX_JAVA_PACKAGE)/Context.java \
            $(GALAX_JAVA_PACKAGE)/Document.java $(GALAX_JAVA_PACKAGE)/Element.java \
            $(GALAX_JAVA_PACKAGE)/ExternalContext.java $(GALAX_JAVA_PACKAGE)/GalapiException.java \
            $(GALAX_JAVA_PACKAGE)/GalaxInternalException.java $(GALAX_JAVA_PACKAGE)/Galax.java \
            $(GALAX_JAVA_PACKAGE)/Item.java $(GALAX_JAVA_PACKAGE)/ItemList.java $(GALAX_JAVA_PACKAGE)/CompiledProgram.java \
            $(GALAX_JAVA_PACKAGE)/CompiledModule.java $(GALAX_JAVA_PACKAGE)/NamespaceEnv.java $(GALAX_JAVA_PACKAGE)/NameValuePair.java \
            $(GALAX_JAVA_PACKAGE)/Node.java $(GALAX_JAVA_PACKAGE)/NodeList.java \
            $(GALAX_JAVA_PACKAGE)/PreparedProgram.java $(GALAX_JAVA_PACKAGE)/ProcessingContext.java $(GALAX_JAVA_PACKAGE)/ProcessingInstruction.java \
            $(GALAX_JAVA_PACKAGE)/Text.java $(GALAX_JAVA_PACKAGE)/xsAnyURI.java \
            $(GALAX_JAVA_PACKAGE)/xsBoolean.java $(GALAX_JAVA_PACKAGE)/xsDecimal.java $(GALAX_JAVA_PACKAGE)/xsDouble.java \
            $(GALAX_JAVA_PACKAGE)/xsFloat.java $(GALAX_JAVA_PACKAGE)/xsInteger.java \
            $(GALAX_JAVA_PACKAGE)/xsQName.java $(GALAX_JAVA_PACKAGE)/xsString.java $(GALAX_JAVA_PACKAGE)/xsUntyped.java \
	    $(GALAX_JAVA_PACKAGE)/xsDate.java  $(GALAX_JAVA_PACKAGE)/xsTime.java   $(GALAX_JAVA_PACKAGE)/xsDateTime.java \
	    $(GALAX_JAVA_PACKAGE)/xsDayTimeDuration.java $(GALAX_JAVA_PACKAGE)/xsYearMonthDuration.java

GALAX_JAVAAPI_JAVAHEADERS=$(GALAX_JAVA_PACKAGE)_Atomic.h $(GALAX_JAVA_PACKAGE)_AtomicList.h \
            $(GALAX_JAVA_PACKAGE)_Attribute.h $(GALAX_JAVA_PACKAGE)_AttributeList.h \
            $(GALAX_JAVA_PACKAGE)_Comment.h $(GALAX_JAVA_PACKAGE)_Context.h \
            $(GALAX_JAVA_PACKAGE)_Document.h $(GALAX_JAVA_PACKAGE)_Element.h \
            $(GALAX_JAVA_PACKAGE)_ExternalContext.h $(GALAX_JAVA_PACKAGE)_GalapiException.h \
            $(GALAX_JAVA_PACKAGE)_GalaxInternalException.h $(GALAX_JAVA_PACKAGE)_Galax.h \
            $(GALAX_JAVA_PACKAGE)_Item.h $(GALAX_JAVA_PACKAGE)_ItemList.h $(GALAX_JAVA_PACKAGE)_CompiledProgram.h \
            $(GALAX_JAVA_PACKAGE)_CompiledModule.h $(GALAX_JAVA_PACKAGE)_NamespaceEnv.h $(GALAX_JAVA_PACKAGE)_NameValuePair.h \
            $(GALAX_JAVA_PACKAGE)_Node.h $(GALAX_JAVA_PACKAGE)_NodeList.h \
            $(GALAX_JAVA_PACKAGE)_PreparedProgram.h $(GALAX_JAVA_PACKAGE)_ProcessingContext.h $(GALAX_JAVA_PACKAGE)_ProcessingInstruction.h \
            $(GALAX_JAVA_PACKAGE)_Text.h $(GALAX_JAVA_PACKAGE)_xsAnyURI.h \
            $(GALAX_JAVA_PACKAGE)_xsBoolean.h $(GALAX_JAVA_PACKAGE)_xsDecimal.h $(GALAX_JAVA_PACKAGE)_xsDouble.h \
            $(GALAX_JAVA_PACKAGE)_xsFloat.h $(GALAX_JAVA_PACKAGE)_xsInteger.h \
            $(GALAX_JAVA_PACKAGE)_xsQName.h $(GALAX_JAVA_PACKAGE)_xsString.h $(GALAX_JAVA_PACKAGE)_xsUntyped.h \
	    $(GALAX_JAVA_PACKAGE)_xsDate.h  $(GALAX_JAVA_PACKAGE)_xsTime.h   $(GALAX_JAVA_PACKAGE)_xsDateTime.h \
	    $(GALAX_JAVA_PACKAGE)_xsDayTimeDuration.h $(GALAX_JAVA_PACKAGE)_xsYearMonthDuration.h

GALAX_JAVAAPI_JAVAHTML=$(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Atomic.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/AtomicList.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Attribute.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/AttributeList.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Comment.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Context.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Document.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Element.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/ExternalContext.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/GalapiException.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/GalaxInternalException.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Galax.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Item.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/ItemList.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/CompiledProgram.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/CompiledModule.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/NamespaceEnv.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/NameValuePair.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Node.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/NodeList.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/PreparedProgram.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/ProcessingContext.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/ProcessingInstruction.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/Text.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsAnyURI.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsBoolean.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsDecimal.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsDouble.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsFloat.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsInteger.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsQName.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsString.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsUntyped.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsDate.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsTime.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsDateTime.html \
         $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsDayTimeDuration.html $(DOCDIR)/$(GALAX_JAVA_PACKAGE)/xsYearMonthDuration.html

GALAX_JAVAAPI_JAVAOBJECTS=$(GALAX_JAVAAPI_JAVASOURCES:.java=.class)

#########################################################################
# Sub-Section: C compilation variables and rules
# Description:
#     These are the variables and rules for building C object files
###########################################################################
CFLAGS+=$(CCFLAGS) $(GALAX_JAVAAPI_INCLUDES)

#########################################################################
# Sub-Section: Targets
# Description:
#     These are the configuration-dependent targets built by "all"
###########################################################################
GALAX_JAVAAPI_TARGETS=$(GALAX_JAVAAPI_ARCHIVE)
ifeq ($(SUPPORTS_SHARED_LIBRARIES), true)
GALAX_JAVAAPI_TARGETS+=$(GALAX_JAVAAPI_OPTSHAREDLIB)
endif

#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the GALAX_JAVAAPI_TARGETS
#  install::   This target does installation for Java API files
#  uninstall:: This target uninstalls Java API files
#  clean::     This target cleans files created in the all section
#########################################################################

all: $(GALAX_JAVAAPI_TARGETS) docs

byte:

install: all $(CONF_GALAX_JAVA_API)
	for i in $(GALAX_JAVAAPI_TARGETS); do ($(CP) $$i $(CONF_GALAX_JAVA_API)); done
	$(CP) $(GALAX_JAVAAPI_ARCHIVE) $(CONF_GALAX_JAVA_API)
	$(CP) -R doc $(CONF_GALAX_JAVA_API)

uninstall:
	for i in $(GALAX_JAVAAPI_TARGETS); do ($(RM) $(CONF_GALAX_JAVA_API)/$$i); done

clean:
	$(RM)  *.class
	$(RM)  $(GALAX_JAVA_PACKAGE)/*.class
	$(RM)  *.cm[oixa]
	$(RM)  *.[oa] *.so *.dll *.dylib .jnilib *~
	$(RM)  *.h
	$(RM)  *.log
	$(RM)  *.jar
	$(RM) -r $(DOCDIR)
	$(RM) -r META-INF

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

galax_jni_stub.o: galax_jni_stub.c $(GALAX_JAVAAPI_JAVAHEADERS) $(GALAX_JAVAAPI_JAVAOBJECTS)
	$(CC) $(CFLAGS) -I../c_api $(JAVAC_INCLUDE) -c galax_jni_stub.c

$(GALAX_JAVAAPI_OPTSHAREDLIB): $(GALAX_JAVAAPI_OPTCOBJECTS)
	$(MKSHAREDLIB) $(MKSHAREDLIBFLAGS) $(LDFLAGS) -o $(GALAX_JAVAAPI_OPTSHAREDLIB) $(GALAX_JAVAAPI_OPTCOBJECTS) $(OCAML_CAPI_OPTLIB_PATHS) $(OCAML_CAPI_OPTLIBS) -lm -ldl $(GALAX_CAPI_SHAREDLIBFLAGS)

$(GALAX_JAVAAPI_ARCHIVE): $(GALAX_JAVAAPI_JAVAOBJECTS) 
	$(JAR) cf $(GALAX_JAVA_PACKAGE).jar $(GALAX_JAVAAPI_JAVAOBJECTS)

docs: $(GALAX_JAVAAPI_JAVAHTML)

$(GALAX_JAVAAPI_JAVAHTML): $(GALAX_JAVAAPI_JAVASOURCES)  
	if test -d doc; then : ; else $(MKDIR) doc; fi
	$(JAVADOC) -d $(DOCDIR) $(GALAX_JAVAAPI_JAVASOURCES)


$(GALAX_JAVA_PACKAGE)_%.h: $(GALAX_JAVA_PACKAGE)/%.class
	$(JAVAH) -classpath . $(GALAX_JAVA_PACKAGE).$*

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################

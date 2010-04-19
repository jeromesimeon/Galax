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
# Descripton:
#		This is a replacement Makefile for the Galax usecases
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

BINDIR=../..
TYPE_KIND=weak
USECASES= ns parts rel seq sgml string tree xmp

ifdef OCAMLOPT
BINEXT=$(OPT)
else
BINEXT=$(BYTE)
endif

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
	$(MAKE) -s tests

tests:
	$(MAKE) -s tests-in

byte:

install: $(CONF_GALAX_USECASES)/xquery10
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):" Makefile-inst > $(CONF_GALAX_USECASES)/xquery10/Makefile
	for i in $(USECASES) xmark; do ($(CP) $${i}_context.xq $${i}_usecase.xq $${i}_usecase.expect $(CONF_GALAX_USECASES)/xquery10); done

uninstall:
	$(RM) $(CONF_GALAX_USECASES)/Makefile
	for i in $(USECASES) xmark; do ($(RM) $(CONF_GALAX_USECASES)/$${i}_context.xq $(CONF_GALAX_USECASES)/xquery10/$${i}_usecase.xq $(CONF_GALAX_USECASES)/xquery10/$${i}_usecase.expect); done

clean:
	$(RM) *_pretty.*
	$(RM) *_pretty_core.*
	$(RM) *.out *.cmp
	$(RM) *.mon
	$(RM) buildbot.diffs

clobber::

#########################################################################
# Sub-Section:  Secondary targets
# Description:
#     These are the supporting targets for "all"
#########################################################################

tests-in:
	$(MAKE) -s clean
	$(MAKE) -s banner
	$(MAKE) -s run-orig
	$(MAKE) -s bootstrap

banner:
	@echo
	@echo "======================================" 
	@echo " Running usecase tests for XQuery 1.0"
	@echo "======================================"

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

%_usecase.out: %_usecase.xq %_context.xq
	($(BINDIR)/glx$(BINEXT) xquery -serialize wf -static-typing on -typing ${TYPE_KIND} -context $*_context.xq $*_usecase.xq >$*_usecase.out; \
	$(MAKE) -k -s check T=$*_usecase E=$*_usecase; )

%_type.out: %_usecase.xq %_context.xq
	(echo "$*: none"; time $(BINDIR)/glx$(BINEXT) xquery -static-typing on -typing none -context $*_context.xq $*_usecase.xq -dynamic off; \
	echo "$*: weak"; time $(BINDIR)/glx$(BINEXT) xquery -static-typing on -typing weak -context $*_context.xq $*_usecase.xq -dynamic off)

run-type:
	@echo "";\
        echo "Running with static typing:" ;\
	echo "===========================" ;\
	echo "";\
	(for t in $(USECASES) xmark; do \
	make -k -s $${t}_type.out ; \
	done)

run-xmark-type:
	@echo "";\
        echo "Running with static typing:" ;\
	echo "===========================" ;\
	echo "";\
	(for t in Q*.xq; do \
	echo "$${t}: none"; time $(BINDIR)/glx$(BINEXT) xquery -static-typing on -typing none -context xmark_context.xq $${t} -dynamic off; \
	echo "$${t}: weak"; time $(BINDIR)/glx$(BINEXT) xquery -static-typing on -typing weak -context xmark_context.xq $${t} -dynamic off; \
	done)

run-orig:
	@echo "";\
        echo "Running original queries:" ;\
	echo "=========================" ;\
	echo "";\
	(for t in $(USECASES) xmark; do \
	make -k -s $${t}_usecase.out ; \
	done)

run-compiled:
	@echo;\
        echo "Running pretty printed queries:" ;\
	echo "===============================" ;\
	echo "";\
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/glx$(BINEXT) xquery -serialize wf -optimization on $${t}_usecase_pretty.xq > $${t}_usecase_pretty.out; \
	$(MAKE) -k -s check T=$${t}_usecase_pretty E=$${t}_usecase; \
	done)

run-compiled-core:
	@echo "";\
        echo "Running pretty printed core:" ;\
	echo "============================" ;\
	echo "";\
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/glx$(BINEXT) xquery -factorization off -execute normalized -serialize wf -optimization on $${t}_usecase_pretty_core.xq > $${t}_usecase_pretty_core.out; \
	$(MAKE) -k -s check T=$${t}_pretty_core E=$${t}_usecase; \
	done)

run-logical-plan: 
	@echo "";\
        echo "Running logical plans:" ;\
	echo "======================" ;\
	echo "";\
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/glx$(BINEXT) xquery -dynamic off -print-plan-kind xml -output-logical-plan $${t}_plan.xml -print-logical-plan on -context $${t}_context.xq $${t}_usecase.xq; \
	$(BINDIR)/glx$(BINEXT) xquery -execute logical $${t}_plan.xml  >$*_usecase.out; \
	$(MAKE) -k -s check T=$*_usecase E=$*_usecase; \
	done)

compile:
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/glx$(BINEXT) compile -print-optimized-plan off -print-prolog on -print-expr on -context $${t}_context.xq $${t}_usecase.xq > $${t}_usecase_pretty.xq; \
	done)

compile-core:
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/glx$(BINEXT) compile -print-optimized-plan off -print-prolog on -print-rewritten-expr on -context $${t}_context.xq $${t}_usecase.xq > $${t}_usecase_pretty_core.xq; \
	done)

compile-double-core:
	(for t in $(USECASES) xmark; \
	do $(BINDIR)/glx$(BINEXT) compile -execute normalized -print-optimized-plan off -print-prolog on -print-normalized-expr on $${t}_usecase_pretty_core.xq > $${t}_usecase_double_pretty_core.xq; \
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

$(CONF_GALAX_USECASES)/xquery10:
	$(MKDIR) $(CONF_GALAX_USECASES)/xquery10

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


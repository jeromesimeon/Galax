#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
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

BINDIR=../..

ifdef OCAMLOPT
BINEXT=$(OPT)
else
BINEXT=$(BYTE)
endif

CONF_GALAX_XQUPDATE_USECASES=$(CONF_GALAX_USECASES)/update10

USECASES= \
parts_q1\
parts_q2\
parts_q3_vs1\
parts_q3_vs2\
parts_q4\
parts_q6\
r\
namespaces\
nil\
soap\
addressbook

tests:
	$(MAKE) -s tests-in

tests-in:
	$(MAKE) -s banner
	$(MAKE) -s clean
	$(MAKE) -s run-orig

banner:
	@echo
	@echo "============================================="
	@echo " Running usecase tests for XQuery Update 1.0"
	@echo "============================================="

%_usecase.out: %_usecase.xq 
	($(CONF_GALAX_BIN)/glx$(BINEXT) xquery -language xquerybang -serialize standard $*_usecase.xq >$*_usecase.out; \
	$(MAKE) -k -s check T=$*_usecase E=$*_usecase; )

install: $(CONF_GALAX_XQUPDATE_USECASES)
	$(CP) Makefile $(CONF_GALAX_XQUPDATE_USECASES)
	for i in $(USECASES); do ($(CP) $${i}_usecase.xq  $${i}_usecase.expect $(CONF_GALAX_XQUPDATE_USECASES)); done

uninstall:
	$(RM) $(CONF_GALAX_XQUPDATE_USECASES)/Makefile
	for i in $(USECASES); do ($(RM) $(CONF_GALAX_XQUPDATE_USECASES)/$${i}_usecase.xq  $(CONF_GALAX_XQUPDATE_USECASES)/$${i}_usecase.expect); done

run-orig:
	@echo "";\
        echo "Running original queries:" ;\
	echo "=========================" ;\
	echo "";\
	(for t in $(USECASES); do \
	make -k -s $${t}_usecase.out ; \
	done)


clean:
	rm -f *.out *.cmp *.mon

$(CONF_GALAX_XQUPDATE_USECASES):
	$(MKDIR) $(CONF_GALAX_XQUPDATE_USECASES)

gen:
	for t in $(USECASES); \
		do $(CONF_GALAX_BIN)/glx$(BINEXT) xquery -language xquerybang -serialize wf $${t}_usecase.xq >$${t}_usecase.expect; \
	done


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

CONF_GALAX_XQUPDATE_USECASES=$(CONF_GALAX_USECASES)/xqupdate

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


DOCFILES=\
bids.xml\
items.xml\
part-list.xml\
part-tree.xml\
users.xml\
airports\
archive.xml\
copy1.xml\
copy2.xml\
employees.xml\
grant.xml\
log.xml\
soap_input.xml

all:
	$(MAKE) clean
	$(MAKE) -s run-orig

tests:
	$(MAKE) all

%_usecase.out: %_usecase.xq 
	($(CONF_GALAX_BIN)/glx$(BINEXT) xquery -language xquerybang -serialize standard $*_usecase.xq >$*_usecase.out; \
	$(MAKE) -k -s check T=$*_usecase E=$*_usecase; )

install: $(CONF_GALAX_XQUPDATE_USECASES) $(CONF_GALAX_XQUPDATE_USECASES)/docs
	$(CP) Makefile $(CONF_GALAX_XQUPDATE_USECASES)
	for i in $(USECASES); do ($(CP) $${i}_usecase.xq  $${i}_usecase.expect $(CONF_GALAX_XQUPDATE_USECASES)); done
	for i in $(DOCFILES); do ($(CP) docs/$$i $(CONF_GALAX_XQUPDATE_USECASES)/docs); done

uninstall:
	$(RM) $(CONF_GALAX_XQUPDATE_USECASES)/Makefile
	for i in $(USECASES); do ($(RM) $(CONF_GALAX_XQUPDATE_USECASES)/$${i}_usecase.xq  $(CONF_GALAX_XQUPDATE_USECASES)/$${i}_usecase.expect); done
	for i in $(DOCFILES); do ($(RM) $(CONF_GALAX_XQUPDATE_USECASES)/docs/$$i); done

run-orig:
	@echo "";\
        echo "Original queries:" ;\
	echo "=================" ;\
	echo "";\
	(for t in $(USECASES); do \
	make -k -s $${t}_usecase.out ; \
	done)


clean:
	rm -f *.out *.cmp *.mon

$(CONF_GALAX_XQUPDATE_USECASES):
	$(MKDIR) $(CONF_GALAX_XQUPDATE_USECASES)

$(CONF_GALAX_XQUPDATE_USECASES)/docs:
	$(MKDIR) $(CONF_GALAX_XQUPDATE_USECASES)/docs

gen:
	for t in $(USECASES); \
		do $(CONF_GALAX_BIN)/glx$(BINEXT) xquery -language xquerybang -serialize wf $${t}_usecase.xq >$${t}_usecase.expect; \
	done


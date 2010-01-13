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
#  all:        This target builds the TARGETS
#  install::   This target does installation for OCaml API example files
#  uninstall:: This target uninstalls OCaml API example files
#  clean::     This target cleans files created in the all section
#########################################################################

all: $(TARGETS)

byte:	$(BYTETARGETS)

tests:
	$(MAKE) -s run-tests

run-tests:
	@echo "";\
        echo "Running Caml API Tests:" ;\
	echo "=======================" ;\
	echo "";\
	$(MAKE) -s $(TESTS)

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

clean::
	$(RM) $(TARGETS)
	$(RM) *.cmx *.cmo *.cmi *.o
	$(RM) *.out *.cmp

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

runexample: example$(BYTE)
	(./example$(BYTE) > example.out; \
	$(MAKE) -k -s check T=$*example E=$*example; )

runtest: test$(BYTE)
	(./test$(BYTE) > test.out; \
	$(MAKE) -k -s check T=$*test E=$*test; )

runoptexample: example$(OPT)
	(./example$(OPT) > optexample.out; \
	$(MAKE) -k -s check T=$*optexample E=$*example; )

runopttest: test$(OPT)
	(./test$(OPT) > opttest.out; \
	$(MAKE) -k -s check T=$*opttest E=$*test; )

#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


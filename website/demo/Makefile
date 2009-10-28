#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.30 2008/02/13 20:18:50 mff Exp $ #

# Web demo Makefile

# Makefile pre-includes

LOCALPREFIX=../..

# User configuration
-include $(LOCALPREFIX)/config/Makefile.galax

# Web site configuration
include ../Makefile.config

###########################################################################
# You should not need to modify below this line

CGITARGET=galax.cgi
OPTCGITARGET=galax$(OPT).cgi

USECASETARGET=usecase$(EXE)
OPTUSECASETARGET=usecase$(OPT)

OBJS=demo_conf.cmo querycgi.cmo 
VIEWOBJS=demo_conf.cmo
USECASEOBJS=demo_conf.cmo usecase.cmo
OPTUSECASEOBJS=$(USECASEOBJS:.cmo=.cmx)
WGALAX_ALL_INCLUDES=$(GALAX_ALL_INCLUDES) -I $(CONF_OCAML_PKGLIB)/netcgi1
                  
all: $(USECASETARGET) $(CGITARGET) 

opt: $(OPTUSECASETARGET) $(OPTCGITARGET) 

$(USECASETARGET): $(USECASEOBJS)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(WGALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(USECASEOBJS)

$(OPTUSECASETARGET): $(USECASEOBJS:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(WGALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(USECASEOBJS:.cmo=.cmx)

$(CGITARGET): $(OBJS)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(WGALAX_ALL_INCLUDES) -I $(CONF_OCAML_PKGLIB)/cgi $(GALAX_ALL_LIBS) $(CONF_OCAML_PKGLIB)/netcgi1/cgi.cma $(OBJS)

$(OPTCGITARGET): $(OBJS:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(WGALAX_ALL_INCLUDES) -I $(CONF_OCAML_PKGLIB)/cgi  $(GALAX_ALL_OPTLIBS) $(CONF_OCAML_PKGLIB)/netcgi1/cgi.cmxa $(OBJS:.cmo=.cmx)
installdocs:
	if test -d $(DEMOSITE); then : ; else $(MKDIR) $(DEMOSITE); fi
	if test -d $(DEMOSITE)/docs; then rm -f $(DEMOSITE)/docs/*: ; else $(MKDIR) $(DEMOSITE)/docs; fi
	echo "DOCUMENT_DIR=$(DEMOSITE)" > galax_demo_config
	echo "CGIBIN_PREFIX=$(CGIBIN_PREFIX)" >> galax_demo_config
	cp galax_demo_config $(DEMOCGIBIN)
	cp maindocuments/*.* $(DEMOSITE)
	cp ../../usecases/*_context.xq $(DEMOSITE)
	cp -R ../../usecases/docs/* $(DEMOSITE)/docs
	(if test -f $(OPTUSECASETARGET); then ./$(OPTUSECASETARGET) ../../usecases $(DEMOSITE) galax_demo_config; else :; fi)
	cp -R ../../usecases/xqupdate/docs/* $(DEMOSITE)/docs
	cp ../../usecases/xqupdate/*_context.xq $(DEMOSITE)
	(if test -f $(OPTUSECASETARGET); then ./$(OPTUSECASETARGET) -language ultf ../../usecases/xqupdate $(DEMOSITE) galax_demo_config; else :; fi)

install: installdocs $(CGITARGET) 
	cp $(CGITARGET) $(DEMOCGIBIN)
	chmod a+x $(DEMOSITE)

installopt: installdocs $(OPTCGITARGET)
	cp $(OPTCGITARGET) $(DEMOCGIBIN)
	chmod a+x $(DEMOSITE)

.depend: *.mli *.ml
	$(OCAMLDEP) *.mli *.ml >.depend

clean:
	rm -f *.cmx *.cmo *.cmi *.o
	rm -f $(CGITARGET) $(OPTCGITARGET)
	rm -f $(USECASETARGET) $(OPTUSECASETARGET)
	rm -f core *~ TAGS
	rm -f galax_demo_config

tags: 
	etags *.ml *.mli *.mll *.mly

include .depend

.SUFFIXES:	.ml .mli .mll .cmo .cmi .cmx .java .class

.mll.ml:
	$(OCAMLLEX) $<

.ml.cmo:
	$(OCAMLC) $(OCAMLC_FLAGS) $(WGALAX_ALL_INCLUDES) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLC_FLAGS) $(WGALAX_ALL_INCLUDES) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) $(WGALAX_ALL_INCLUDES) -c $<


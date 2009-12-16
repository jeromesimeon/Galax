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

TARGETNAME1=galax
TARGET1=$(TARGETNAME1)$(EXE)
OPTTARGET1=$(TARGETNAME1)$(OPT)
BYTETARGET1=$(TARGETNAME1)$(BYTE)
OPTPROFTARGET1=$(TARGETNAME1).optprof
TARGETOBJS1=toplevel/galax_main.cmo

TARGETNAME4=galax-mapwsdl
TARGET4=$(TARGETNAME4)$(EXE)
OPTTARGET4=$(TARGETNAME4)$(OPT)
BYTETARGET4=$(TARGETNAME4)$(BYTE)
TARGETOBJS4=toplevel/$(TARGETNAME4).cmo

TARGETNAME6=xquery2soap
TARGET6=$(TARGETNAME6)$(EXE)
OPTTARGET6=$(TARGETNAME6)$(OPT)
BYTETARGET6=$(TARGETNAME6)$(BYTE)
TARGETOBJS6=toplevel/$(TARGETNAME6).cmo

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
$(TARGETNAME4) \
$(TARGETNAME6) \
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
$(BYTETARGET4) \
$(BYTETARGET6) \
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
$(TARGETOBJS4) \
$(TARGETOBJS6) \
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
$(OPTTARGET4) \
$(OPTTARGET6) \
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

$(BYTETARGET4):	$(GALAX_LIB) $(TARGETOBJS4)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS4)

$(BYTETARGET6):	$(GALAX_LIB) $(TARGETOBJS6)
	$(OCAMLC) -custom -linkall -o $@ $(OCAMLC_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_LIBS) $(TARGETOBJS6)

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

$(OPTTARGET4):	$(GALAX_OPTLIB) $(TARGETOBJS4:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS4:.cmo=.cmx)

$(OPTTARGET6):	$(GALAX_OPTLIB) $(TARGETOBJS6:.cmo=.cmx)
	$(OCAMLOPT) -linkall -o $@ $(OCAMLOPT_FLAGS) $(GALAX_ALL_INCLUDES) $(GALAX_ALL_OPTLIBS) $(TARGETOBJS6:.cmo=.cmx)

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

## Website
#

install::

uninstall::

clean::
	cd website ; $(MAKE) clean

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


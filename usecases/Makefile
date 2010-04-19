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
# Author:	Christopher A. Rath (AT&T Labs Research)
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
LOCALPREFIX=..

include $(LOCALPREFIX)/config/Makefile.galax

#########################################################################
# Section:	Local variables
# Description:
#		These variables are used for targets of this Makefile
###########################################################################

BINDIR=..
USECASES=xquery10 update10

ifdef OCAMLOPT
BINEXT=$(OPT)
else
BINEXT=$(BYTE)
endif

DOCFILES=\
auction.xml \
bids.xml \
book1.xml \
books.xml \
company-data.xml \
company.dtd \
herpo.xsd \
hispo.xsd \
items.xml \
parts.xml \
prices.xml \
report1.xml \
sgml.xml \
string.xml \
users.xml \
xmark.xml \
xmpbib.xml \
xmpreviews.xml \
part-list.xml\
part-tree.xml\
airports\
archive.xml\
copy1.xml\
copy2.xml\
employees.xml\
grant.xml\
log.xml\
soap_input.xml

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

tests:
	for i in $(USECASES); do (cd $${i}; $(MAKE) -s tests); done

byte:

install: $(CONF_GALAX_USECASES) $(CONF_GALAX_USECASES)/docs
	$(SED) -e "s:CONF_GALAX_CONFIG:$(CONF_GALAX_CONFIG):" Makefile-inst > $(CONF_GALAX_USECASES)/Makefile
	for i in $(USECASES); do ($(MKDIR) $(CONF_GALAX_USECASES)/$${i}; cd $${i}; $(MAKE) install); done
	for i in $(DOCFILES); do ($(CP) docs/$$i $(CONF_GALAX_USECASES)/docs); done

uninstall:
	$(RM) $(CONF_GALAX_USECASES)/Makefile
	for i in $(USECASES); do ($(RM) $(CONF_GALAX_USECASES)/$${i}_context.xq $(CONF_GALAX_USECASES)/$${i}_usecase.xq $(CONF_GALAX_USECASES)/$${i}_usecase.expect); done
	for i in $(DOCFILES); do ($(RM) $(CONF_GALAX_USECASES)/docs/$$i); done

clean:
	for i in $(USECASES); do (cd $${i}; $(MAKE) clean); done

clobber::


#########################################################################
# Section:  Makefile post-includes
# Description:
#		There are currently no post-includes for this Makefile
#########################################################################


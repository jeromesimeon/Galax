#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.20 2007/02/01 22:08:46 simeon Exp $ #

#########################################################################
# Section:  Default target
# Description:
#     The default target for this Makefile is "all"
#########################################################################

default: all

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
LOCALPREFIX=..

include $(LOCALPREFIX)/config/Makefile.galax

#########################################################################
# Section:  Main targets
# Description:
#     This is where all of the named toplevel targets are placed.
#
#  all:        This target builds the GALAX_CAPI_TARGETS
#  install::   This target does installation for C API files
#  uninstall:: This target uninstalls C API files
#  clean::     This target cleans files created in the all section
#########################################################################

all:
	for i in $(GALAX_EXAMPLE_DIRS); do (cd $$i ; $(MAKE) all); done

byte:
	for i in $(GALAX_EXAMPLE_DIRS); do (cd $$i ; $(MAKE) byte); done

install:
	for i in $(GALAX_EXAMPLE_DIRS); do (cd $$i ; $(MAKE) install); done

uninstall:
	for i in $(GALAX_EXAMPLE_DIRS); do (cd $$i ; $(MAKE) uninstall); done

clean:
	for i in $(GALAX_EXAMPLE_DIRS); do (cd $$i ; $(MAKE) clean); done

clobber::
	for i in $(GALAX_EXAMPLE_DIRS); do (cd $$i ; $(MAKE) clobber); done

#########################################################################
# Section:  Makefile post-includes
# Description:
#     This is where the dependency file is included and generated
#     We place the dependencies to make sure that various files
#     are generated before we need them.
#########################################################################

updatedepend::
	for i in $(GALAX_EXAMPLE_DIRS); do (cd $$i ; $(MAKE) updatedepend); done


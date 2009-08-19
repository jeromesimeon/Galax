#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.29 2007/02/01 22:08:54 simeon Exp $ #

#
# Makefile for pre-compilation tools
#

start:
	$(MAKE) smart

LOCALPREFIX=..
include $(LOCALPREFIX)/config/Makefile
include $(LOCALPREFIX)/config/Makefile.gen

.PHONY: jabber stemmer

smart:
	set -e; (for d in $(REQUIREDTOOLS); do ($(MAKE) $$d); done)
	set -e; (for d in $(REALTOOLS); do ($(MAKE) $$d); done)

#smart:
# 	$(MAKE) ucs2_to_utf8_galax
# 	$(MAKE) http_galax
# 	$(MAKE) glx_curl_galax
# 	$(MAKE) escaping_galax
# 	$(MAKE) unix2dos_galax

ucs2_to_utf8_galax:
	$(MAKE) -C ucs2_to_utf8 all

http_galax:
	$(MAKE) -C http smart

stemmer:
	$(MAKE) -C stemmer smart

curl:
	$(MAKE) -C glx_curl smart

jungle:
	$(MAKE) -C Jungle smart

apache:

escaping_galax:
	$(MAKE) -C escaping all

unix2dos_galax:
	$(MAKE) -C unix2dos all

jabber:
	$(MAKE) -C jabber smart

clean:
	$(MAKE) -C ucs2_to_utf8 clean
	$(MAKE) -C http clean
	$(MAKE) -C glx_curl clean
	$(MAKE) -C Jungle clean
	$(MAKE) -C escaping clean
	$(MAKE) -C unix2dos clean
	$(MAKE) -C jabber clean
	$(MAKE) -C stemmer clean

depend:
	$(MAKE) -C ucs2_to_utf8 depend
	$(MAKE) -C glx_curl depend
	$(MAKE) -C Jungle depend
	$(MAKE) -C stemmer depend

installucs2_to_utf8_galax:

installhttp_galax:
	cd http ; $(MAKE) install

installcurl:
	cd glx_curl ; $(MAKE) install

installjungle:
	cd Jungle ; $(MAKE) install

installstemmer:
	cd stemmer; $(MAKE) install

installescaping_galax:

installunix2dos_galax:


installjabber:
	cd jabber ; $(MAKE) install

installapache:

install:
	set -e; (for d in $(REALTOOLS); do ($(MAKE) install$$d); done)


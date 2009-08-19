#########################################################################
#                                                                       #
#                                  GALAX                                #
#                               XQuery Engine                           #
#                                                                       #
#   Copyright 2001-2007.                                                #
#   Distributed only by permission.                                     #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.25 2008/03/12 22:30:58 simeon Exp $ #

# Web demo Makefile

start:
	$(MAKE) world

LOCALPREFIX=..
include $(LOCALPREFIX)/config/Makefile.galax

include Makefile.config


###########################################################################
# You should not need to modify below this line

world: 
	$(MAKE) opt

opt: 
	(cd demo; $(MAKE) opt)

install: 
	$(MAKE) install-front
	$(MAKE) install-demo
	$(MAKE) install-documentation
	chmod -R a+r $(WEBSITE)

install-documentation:
	if test -d $(WEBSITE)/doc; then : ; else $(MKDIR) $(WEBSITE)/doc; fi
	(cd ../doc; $(MAKE) install-website)

install-demo:
	(cd demo; $(MAKE) installopt)

install-front:
	if test -d $(WEBSITE); then : ; else $(MKDIR) $(WEBSITE); fi
	if test -d $(DEMOSITE); then : ; else $(MKDIR) $(DEMOSITE); fi
	if test -d $(DEMOCGIBIN); then : ; else $(MKDIR) $(DEMOCGIBIN); fi
	cp *.html $(WEBSITE)
	if test -d $(WEBSITE)/doc; then : ; else $(MKDIR) $(WEBSITE)/doc; fi
	cp $(CONF_GALAX_MAN)/*.html $(WEBSITE)/doc
	cp $(CONF_GALAX_MAN)/*.gif $(WEBSITE)/doc
	cp $(CONF_GALAX_MAN)/manual.pdf $(WEBSITE)/doc
	cp *.css $(WEBSITE)
	cp ../LICENSE $(WEBSITE)
	cp -r images $(WEBSITE)
	cp -r optimization $(WEBSITE)
	chmod a+x $(WEBSITE)/images
	chmod a+x $(WEBSITE)/optimization

#	chmod a+x $(WEBSITE)/slides
#	cp -r slides $(WEBSITE)

clean:
	(cd demo; $(MAKE) clean)


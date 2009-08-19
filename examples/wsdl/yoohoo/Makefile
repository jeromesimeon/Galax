GALAX=/home/mff/galax
GALAXHOME=/home/mff/Galax
APACHEHOME=/var/www/html

### SIGMOD DEMO
all: yoohoo-server.xqs

yoohoo-server.xqs : yoohoo-server.xq
	$(GALAXHOME)/bin/xquery2soap -wsdl YooHoo.wsdl -nms yoohoo ./yoohoo-server.xq

yoohoo-server.xq : YooHoo.wsdl
	$(GALAXHOME)/bin/galax-mapwsdl -s yoohoo-server.xq YooHoo.wsdl -impl $(APACHEHOME)/yoohoo-impl.xq -namespace yoohoo

install: YooHoo.wsdl yoohoo-server.xqs yoohoo-impl.xq
	cp YooHoo.wsdl yoohoo-server.xqs yoohoo-impl.xq $(APACHEHOME)
	(cd $(GALAX)/usecases; \
         cp -f $(GALAX)/examples/wsdl/yoohoo/yoohoo_context.xq .; \
         cp -f $(GALAX)/examples/wsdl/yoohoo/yoohoo_usecase.xq .; \
         cp -f $(GALAX)/examples/wsdl/yoohoo/yoohoo_documents.txt .; \
         cd $(GALAX)/usecases/docs; \
         cp -f $(GALAX)/examples/wsdl/yoohoo/yoohoo-server.xq .; \
         cp -f $(GALAX)/examples/wsdl/yoohoo/yoohoo-server.xqs .; \
         cp -f $(GALAX)/examples/wsdl/yoohoo/yoohoo-impl.xq .) 

clean:
	rm -f yoohoo-server*.xqs yoohoo-server*.xq yoohoo-client.xq

### OTHER
yoohoo-server-other.xq : YooHooOther.wsdl
	$(GALAXHOME)/bin/galax-mapwsdl -s yoohoo-server-other.xq YooHooOther.wsdl -impl $(APACHEHOME)/yoohoo-impl-other.xq -namespace yoohoo

yoohoo-server-other.xqs : yoohoo-server-other.xq
	$(GALAXHOME)/bin/xquery2soap -wsdl YooHoo.wsdl -nms yoohoo ./yoohoo-server.xq

install-other: yoohoo-server-other.xqs yoohoo-impl-other.xq
	cp yoohoo-server-other.xqs yoohoo-impl-other.xq $(APACHEHOME)

yoohoo-client.xq:  YooHoo.wsdl
	$(GALAXHOME)/bin/galax-mapwsdl -c yoohoo-client.xq YooHoo.wsdl -namespace yoohoo

startserver:
	(export LD_LIBRARY_PATH=$(GALAXHOME)/lib/c; /usr/local/apache_1.3.31/bin/apachectl start&)
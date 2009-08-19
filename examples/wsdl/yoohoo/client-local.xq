import module namespace yoohoo = "urn:YooHoo" at "/home/mff/galax-0.6.0/examples/wsdl/yoohoo/yoohoo-impl.xq";

yoohoo:status()
(:
yoohoo:accounts();
yoohoo:presence("mary")
yoohoo:presence("jerome")
yoohoo:presence("nicola")
:)


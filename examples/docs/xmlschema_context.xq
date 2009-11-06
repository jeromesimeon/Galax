(: --------------------------------------
       Use Case "XMLS" : XML Schema Primer
     -------------------------------------- :)
declare namespace xmlschema = "test";

import schema namespace HisPo = "http://www.hispo.com/" at "../docs/hispo.xsd";

declare variable $xmlschema:po := validate { fn:doc("../docs/hispo.xml") };



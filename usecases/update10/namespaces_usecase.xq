(: Q1: Moving elements between namespaces. :)
(: Q1:
An agriculture company and an university research lab are making a joint proposal to the National Agricultural Research Agency. An initial proposal has been made by cut and paste out of snippets provided by the two partners. Before being submitted, the proposal has to be moved to the National Agricultural Research Agency (NARA) namespace.
:)
declare namespace nara = "http://www.anr.fr/nara";

for $e in doc("../docs/grant.xml")//*
where not (namespace-uri($e) eq "http://www.anr.fr/nara")
return 
  rename node $e 
      as QName("http://www.anr.fr/nara", 
               concat("nara:",local-name($e)))
;
doc("../docs/grant.xml")

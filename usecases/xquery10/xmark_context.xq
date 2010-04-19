(: ---------------------------------------
       Use Case "XMark" : XML Benchmark

       Thanks to our XMark friends!
       http://monetdb.cwi.nl/xml/
     --------------------------------------- :)

declare variable $auction := doc("../docs/xmark.xml");

declare function local:convert($v as xs:decimal?) as xs:decimal?
{
  2.20371 * $v (: convert Dfl to Euro :)
};


module namespace Galaxd = "galaxd";
declare variable $summary := (: collection("log.xml") :)
 (<entry><query>50</query><response>500</response></entry>,
  <entry><query>60</query><response>600</response></entry>);

declare function Galaxd:stats() { 
  $summary
};

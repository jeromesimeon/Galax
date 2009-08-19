module namespace Galaxd = "galaxd";
declare variable $summary := (: collection("log.xml") :)
 (<entry><query>10</query><response>100</response></entry>,
  <entry><query>20</query><response>200</response></entry>,
  <entry><query>30</query><response>300</response></entry>,
  <entry><query>40</query><response>400</response></entry>);

declare function Galaxd:stats() { 
  $summary
};

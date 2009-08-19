declare default element namespace "http://www.w3.org/2005/02/query-test-XQTSResult";

declare boundary-space preserve;

declare variable $unit external;

declare variable $new :=
  let $r := fn:concat("testresults-",$unit,".xml")
  return doc($r);
declare variable $old :=
  let $r := fn:concat("testresults-",$unit,"-reference.xml")
  return doc($r);

declare function local:summary($n,$o) {
let $npassed := $n//test-case[@result="pass"]
let $nfailed := $n//test-case[@result="fail"]
let $opassed := $o//test-case[@result="pass"]
let $ofailed := $o//test-case[@result="fail"]
return
<summary>
    <passed expected="{count($opassed)}" actual="{count($npassed)}"/>
   <failed expected="{count($ofailed)}" actual="{count($nfailed)}"/>
  </summary>
};

declare function local:same($n,$o,$r) {
let $passed := $r[@result="pass"]
let $failed := $r[@result="fail"]
return
<none xmlns="http://www.w3.org/2005/02/query-test-XQTSResult" comment="BRAVO! ^_^">
  {local:summary($n,$o)}
</none>
};

declare function local:differ($n,$o,$r) {
let $passed := $r[@result="pass"]
let $failed := $r[@result="fail"]
return
<diff xmlns="http://www.w3.org/2005/02/query-test-XQTSResult">
  {local:summary($n,$o)}
  <more-passed count="{count($passed)}">
    {$passed}
  </more-passed>
  <more-failed count="{count($failed)}">
    {$failed}
  </more-failed>
</diff>
};

let $r :=
   for $n in $new//test-case
   for $o in $old//test-case
   where $n/@name = $o/@name
   return
     if ($n/@result ne $o/@result) then $n else ()
return
  if (empty($r)) then local:same($new,$old,$r) else local:differ($new,$old,$r)


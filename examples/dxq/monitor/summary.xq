module namespace Summary = "galaxd";
import module namespace Galaxd = "galaxd" at "./examples/dxq/monitor/b.xq";

declare function local:summary() { 
  let $b := glx:remote-plan("b",0,Galaxd:stats())/entry,
      $c := glx:remote-plan("c",0,Galaxd:stats())/entry
  return 
<summary>
  <count>{ count(($b,$c)) }</count>
  <total-query>{ sum(($b/query, $c/query)) }</total-query>
  <total-response>{ sum(($b/response, $c/response)) }</total-response>
</summary>
};


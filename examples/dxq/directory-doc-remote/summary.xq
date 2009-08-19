module namespace Summary = "server";
import module namespace S = "dxq:server" at "s-int.xq";

declare function local:summary() {
  let $a := exec {"a"}{S:stats()}/entry,
      $b := exec {"b"}{S:stats()}/entry,
      $c := exec {"c"}{S:stats()}/entry,
      $d := exec {"d"}{S:stats()}/entry,
      $e := exec {"e"}{S:stats()}/entry
  return
    <summary>
      <count>{ count(($a,$b,$c,$d,$e)) }</count>
      <total-query> {
        sum(($a/query, $b/query, $c/query, $d/query, $e/query))
      } </total-query>
      <total-response> {
        sum(($a/response, $b/response, $c/response, $d/response, $e/response))
      } </total-response>
    </summary>
};

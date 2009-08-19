(:
  DNS functions implemented at resolver. 
:)
module namespace D = "dns";
import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";

declare function D:init() { () };

(: Resolution using forwarding :)
declare updating function D:resolved($n, $addrs) {
  dxq:gui-report(
    fn:concat(fn:string-join(("Resolved", $n, "to", ""), " "),
      fn:string-join($addrs/@address, " ")))
};



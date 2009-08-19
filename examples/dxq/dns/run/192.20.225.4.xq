module namespace D = "dns";
import interface namespace S = "dns" at "file:dns.xqi";
import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";
declare namespace N = "graph";
declare variable $local:self := "192.20.225.4";
declare variable $local:rr := 
<rr>        
  <soa domain="research.att.com" nameserver="ns.research.att.com" />
  <a hostname="www.research.att.com" address="192.20.3.54" ttl="3600"/>
  <ns domain="." nameserver="a.root-servers.net" />
  <a hostname="a.root-servers.net" address="198.41.0.4" ttl="3600"/>
</rr>
;
declare function D:RR() {
  $local:rr
};

(: Resolution using delegation :)
declare updating function D:delegate($n) {
  <rr>{ 
   (D:RR()/a[@hostname=$n],
    if (dxq:dns-lt(D:RR()/soa/@domain,$n)) then
      for $ns in D:RR()/ns, $a in D:RR()/a
      where $ns/@nameserver=$a/@hostname
      and fn:not($ns/@domain=".")
      and dxq:dns-lt($ns/@domain,$n)
      return 
        let server T implement S at $a/@address 
        return T:delegate($n)/a
     else 
       let $root := D:RR()/a[@hostname = D:RR()/ns[@domain = "."]/@nameserver] return {
(:       dxq:gui-report(fn:string-join(("Root is", $root/@address), "")); :)
       let server T implement S at $root/@address return 
       from server T return T:delegate($n)} 
   )
  }</rr>
};

(: Resolution using forwarding :)
declare updating function D:resolved($n, $addrs) {
  dxq:gui-report(
    fn:concat(fn:string-join(("Resolved", $n, "to"), " "),
      fn:string-join($addrs/@address, " ")))
};

(: Resolution using forwarding :)
declare updating function D:forward($from,$n) {
  let $addrs :=  D:RR()/a[@hostname=$n]
  return {
    if (dxq:dns-lt(D:RR()/soa/@domain,$n)) then
      for $ns in D:RR()/ns, $a in D:RR()/a
      where $ns/@nameserver=$a/@hostname
            and fn:not($ns/@domain=".")
            and dxq:dns-lt($ns/@domain,$n)
      return 
        let server U implement S at $a/@address return 
        at server U do U:forward($from,$n)
    else 
      let $root := D:RR()/a[@hostname = D:RR()/ns[@domain = "."]/@nameserver] return {
(:       dxq:gui-report(fn:string-join(("Root is", $root/@address),""));  :)
        let server T implement S at $root/@address return 
        at server T do T:forward($from,$n)
      };
    if (fn:not(fn:empty($addrs))) then 
      let server T implement S at $from return 
      at server T do T:resolved($n,$addrs)
    else ()
  }
};

(: Multicast :)
declare updating function D:deliver($from,$msg) { 
  let $msg := fn:string-join(($local:self,"received",$msg,"from",$from), " ") 
  return dxq:gui-report($msg);

  (: Deliver message to all terminal hosts in this server's domain :)
  for $a in D:RR()/a
  where fn:empty(D:RR()/ns[@nameserver=$a/@hostname])
        and dxq:dns-lt(D:RR()/soa/@domain,$a/@hostname)
  return 
    let server T implement S at $a/@address 
    return 
      at server T do T:multicast($local:self,$msg)
};

declare updating function D:multicast($from, $x) {
  (: Deliver message to self :)
  D:deliver($from, $x),
  
  (: Deliver message to nameservers in this server's domain :)
  for $ns in D:RR()/ns, $a in D:RR()/a
  where $ns/@nameserver=$a/@hostname
        and dxq:dns-lt(D:RR()/soa/@domain,$ns/@domain)
  return 
    let server T implement S at $a/@address 
    return
      at server T do T:multicast($local:self, $x)
};

declare function N:graphEdge($s,$t,$l,$w) { 
  <edge s="{$s}" t="{$t}" l="{$l}" w="{$w}"/>
};

declare function N:neighborGraph() { 
  <graph name="Neighbors">{
    (for $ns in D:RR()/ns, $a in D:RR()/a  
    where $ns/@nameserver=$a/@hostname  
          and fn:not($ns/@domain=".") 
    return 
      N:graphEdge($local:self,$a/@address,$ns/@domain,2)),
    (for $a in D:RR()/a  
     where fn:empty(D:RR()/ns[@nameserver = $a/@hostname])
     return 
      N:graphEdge($local:self,$a/@address,"client",2))
  }</graph>
};

declare updating function N:reportNeighborGraph() {
  dxq:gui-status("l", N:neighborGraph())
};

declare updating function D:init() { 
  N:reportNeighborGraph()
};

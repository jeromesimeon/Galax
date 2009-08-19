(: A DNS resolver.

   If $x is the IP address of a nameserver,
   and $n is a hostname, then

     R:lookup($x,$n)

   should return all the address records for $n.
:)

module namespace R = "resolver";
import module namespace DNS = "dns" at "../resolver-dns.xq";
import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";
import interface namespace S = "dns" at "dns.xqi"; 

declare variable $local:self := "resolver";
declare server Root implements S at "198.41.0.4";

declare function R:lookup($x,$n) {
  let server T implement S at $x return 
  <rr>{ 
     (T:RR()/a[@hostname=$n],
      for $ns in T:RR()/ns, $a in T:RR()/a
      let $z := $a/@address
      where $ns/@nameserver=$a/@hostname
            and fn:not($ns/@domain=".")
            and dxq:dns-lt($ns/@domain,$n)
      return
        R:lookup($z,$n)/a)
  }</rr>
};

declare function R:resolve($n) { 
  R:lookup("198.41.0.4",$n)
};

declare function R:lookup-opt($x,$n) {
  let server T implement S at $x return 
  let $temp := from server T return 
   <temp>
     <addrs>{ T:RR()/a[@hostname=$n] }</addrs>
     <server>{
        for $ns in T:RR()/ns, $a in T:RR()/a
        let $z := $a/@address
        where $ns/@nameserver=$a/@hostname
              and fn:not($ns/@domain=".")
              and dxq:dns-lt($ns/@domain,$n)
        return $z
      }</server>
   </temp>
  return 
  <rr>{ 
    ($temp/addrs/*,
     for $z in $temp/server/@address 
     return R:lookup-opt($z,$n)/a)
  }</rr>
};

declare function R:resolve-opt($n) { 
  R:lookup-opt("198.41.0.4",$n)
};

declare updating function R:delegate($n) { 
  Root:delegate($n)
};

declare updating function R:forward($n) { 
  at server Root do Root:forward("resolver", $n)
};

declare updating function R:multicast($n) { 
  at server Root do Root:multicast("resolver", $n)
};




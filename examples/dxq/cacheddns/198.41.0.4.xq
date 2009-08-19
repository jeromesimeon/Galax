(: Root nameserver :)
module namespace D = "dns";
import module namespace S = "dxq:dns" at "dns.xqi";

declare function D:RR() {
  <rr>        
    <soa domain="." nameserver="a.root-servers.net" />
    <ns domain="com" nameserver="a.gtld-servers.net" />
    <a hostname="a.gtld-servers.net" address="198.41.3.38" ttl="3600"/>
  </rr>
};

(: Peer-to-peer :)
declare function D:down($n) {
  <rr>{ 
    D:RR()/a[@hostname=$n],
    for $ns in D:RR()/ns, $a in D:RR()/a
    where $ns/@nameserver=$a/@hostname
    and fn:not($ns/@domain=".")
    and glx:dns-lt($ns/@domain,$n)
    return exec { $a/@address } { S:down($n)/a }
  }</rr>
};

(: multicast :)
declare function D:deliver($x) { () };
declare function D:multicast($x) {
  D:deliver($x),        
  for $ns in D:RR()/ns, $a in D:RR()/a
  where $ns/@nameserver=$a/@hostname
  and glx:dns-lt(D:RR()/soa/@domain,$ns/@domain)
  return exec { $a/@address } [ S:multicast($x) ]
};

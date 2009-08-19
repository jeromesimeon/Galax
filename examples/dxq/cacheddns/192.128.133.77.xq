(: att.com nameserver :)
module namespace D = "dns";
import module namespace S = "dxq:dns" at "dns.xqi";

declare function D:RR() {
  <rr>    
    <soa domain="att.com" nameserver="kcgw1.att.com" />

    <ns domain="research.att.com" nameserver="ns.research.att.com" />
    <a hostname="ns.research.att.com" address="192.20.225.4" ttl="3600"/>

    <a hostname="www.att.com" address="192.20.3.54" ttl="3600"/>

    <ns domain="." nameserver="a.root-servers.net" />
    <a hostname="a.root-servers.net" address="198.41.0.4" ttl="3600"/>

    <a hostname="www.research.att.com" address="192.20.3.54" ttl="1"/>
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

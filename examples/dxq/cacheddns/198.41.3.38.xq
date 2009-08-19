(: com nameserver :)
module namespace D = "dns";
import module namespace S = "dxq:dns" at "dns.xqi";

declare function D:RR() {
  <rr>        
    <soa domain="com" nameserver="a.gtld-servers.net" />

    <ns domain="att.com" nameserver="kcgw1.att.com" />
    <a hostname="kcgw1.att.com" address="192.128.133.77" ttl="3600"/>

    <ns domain="ibm.com" nameserver="ns.almaden.ibm.com" />
    <a hostname="ns.almaden.ibm.com" address="198.4.83.35" ttl="3600"/>

    <ns domain="." nameserver="a.root-servers.net" />
    <a hostname="a.root-servers.net" address="198.41.0.4" ttl="3600"/>

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

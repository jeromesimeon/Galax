(: A DNS resolver.

   If $x is the IP address of a nameserver,
   and $n is a hostname, then

     R:lookup($x,$n)

   should return all the address records for $n.
:)

module namespace R = "resolver";
import module namespace S = "dxq:dns" at "dns.xqi";

declare function R:id($x) { $x };

declare function R:f($x) {
  <rr>{ 
      exec { $x } { S:RR()/a },
      for $ns in exec { $x } { S:RR()/ns }
      for $a in exec { $x } { S:RR()/a }
      where $ns/@nameserver=$a/@hostname
      return R:id($a)
  }</rr>
};

declare function R:lookup($x,$n) {
  <rr>{ 
      exec { $x } { S:RR()/a[@hostname=$n] },
      for $ns in exec { $x } { S:RR()/ns }
      for $a in exec { $x } { S:RR()/a }
      let $z := $a/@address
      where $ns/@nameserver=$a/@hostname
      and fn:not($ns/@domain=".")
      and glx:dns-lt($ns/@domain,$n)
      return
      R:lookup($z,$n)/a
  }</rr>
};

(: ---- test functions ---- :)
declare function R:test($x) {
  <rr>
    { exec { $x } { S:RR()/a } }
  </rr>
};

declare function R:test1($x) {
  <rr>
    { for $ns in exec { $x } { S:RR()/ns }
      for $a in exec { $x } { S:RR()/a }
      return $a 
    }
  </rr>
};

(: A DNS resolver.

   If $x is the IP address of a nameserver,
   and $n is a hostname, then

     R:lookup($x,$n)

   should return all the address records for $n.
:)

module namespace R = "resolver";
import module interface S = "dns" at "dns.xqi";

declare function R:id($x) { $x };

declare function R:f($x) {
  <rr>{
      exec bind loc with S { S:RR()/a },
      for $ns in exec bind $x with S { S:RR()/ns }
      for $a in exec bind $x with S { S:RR()/a }
      where $ns/@nameserver=$a/@hostname
      return R:id($a)
  }</rr>
};

declare function R:lookup($x,$n) {
  <rr>{ 
      exec bind $x with S { S:RR()/a[@hostname=$n] },
      for $ns in exec $x with S { S:RR()/ns }
      for $a in exec $x with S { S:RR()/a }
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
    { exec $x with S { S:RR()/a } }
  </rr>
};

declare function R:test1($x) {
  <rr>
    { for $ns in exec $x with S { S:RR()/ns }
      for $a in exec $x with S { S:RR()/a }
      return $a 
    }
  </rr>
};

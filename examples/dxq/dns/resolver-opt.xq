(: A DNS resolver.

   If $x is the IP address of a nameserver,
   and $n is a hostname, then

     R:lookup($x,$n)

   should return all the address records for $n.
:)

module namespace R = "resolver";
import module namespace S = "dxq:dns" at "dns.xqi";

(: Push path expressions only :)
declare function R:lookup($x,$n) {
 let server T implement S at $x return { 
  <rr>{ 
      from server T return { T:RR()/a[@hostname=$n] },
      for $ns in from server T return { T:RR()/ns }
      for $a in from server T return { T:RR()/a }

      where $ns/@nameserver=$a/@hostname
      and fn:not($ns/@domain=".")
      and glx:dns-lt($ns/@domain,$n)
      return
      R:lookup($a/@address,$n)/a
  }</rr>
 }
};

(: Local Join :)
declare function R:lookup2($x,$n) {
 let server T implement S at $x return { 
  <rr>{ 
  let $rr := from server T return { T:RR() }
  return
      ($rr/a[@hostname=$n], 
      for $ns in $rr/ns, $a in $rr/a
      where $ns/@nameserver=$a/@hostname
      and fn:not($ns/@domain=".")
      and glx:dns-lt($ns/@domain,$n)
      return
      R:lookup2($a/@address,$n)/a)
  }</rr>
 }
};

(: The idea here to define a function that build all resource records
   at one site, then have optimization that allows us to query a specific
   hostname and only query necessary sites.  Doesn't work with current
   optimizations.

   There is a common paradigm here where data is partitioned across nodes
   hierarchically based on domain/some index, and we would like to
   automatically figure out what partition(s) to visit.
:)
declare function R:allRR($x,$domain) {
 let server T implement S at $x return { 
  let $rr := from server T return { T:RR() } return
  let $childRR := 
    for $ns in $rr/ns, $a in $rr/a
    where $ns/@nameserver=$a/@hostname
    and glx:dns-lt($domain,$ns/@domain)
    return R:allRR($a/@address,$ns/@domain)
  return
  let $rr1 := $rr/a[glx:dns-le($domain,@hostname)]
  return
  let $childrr1 := $childRR[glx:dns-le($domain,@hostname)]
  return ($rr1,$childrr1)
 }
};
declare function R:lookupN($n) {
  R:allRR("198.41.0.4",".")[@hostname=$n]
};

(: Push join to one site :)
declare function R:lookup3($x,$n) {
 let server T implement S at $x return { 
  <rr>{ 
  from server T return { T:RR()/a[@hostname=$n] },
  for $a in 
    from server T return { 
      for $ns in T:RR()/ns,
          $a in T:RR()/a
      where $ns/@nameserver=$a/@hostname
      and fn:not($ns/@domain=".")
      and glx:dns-lt($ns/@domain,$n)
      return $a 
    }
   return
   R:lookup3($a/@address,$n)/a
  }</rr>
 }
};

(: Simulate pushing a tuple plan :)
declare function R:lookup4($x,$n) {
 let server T implement S at $x return { 
  <rr>{ 
  let $tuples := 
    from server T return { 
      (<hosts>{ T:RR()/a[@hostname=$n] }</hosts>,
       for $ns in T:RR()/ns,
           $a in T:RR()/a
       where $ns/@nameserver=$a/@hostname
       and fn:not($ns/@domain=".")
       and glx:dns-lt($ns/@domain,$n)
       return $a)
    }
  return
   ($tuples/self::hosts/a,
    for $a in $tuples/self::a return
    R:lookup4($a/@address,$n)/a)
  }</rr>
 }
};

(: ---- test functions ---- :)
declare function R:test($x) {
 let server T implement S at $x return { 
  <rr>
    { from server T return { T:RR()/a } }
  </rr>
 }
};

declare function R:test1($x) {
 let server T implement S at $x return { 
  <rr>
    { for $ns in from server T return { T:RR()/ns }
      for $a in from server T return { T:RR()/a }
      return $a 
    }
  </rr>
 }
};

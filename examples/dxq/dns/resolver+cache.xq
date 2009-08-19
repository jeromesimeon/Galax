(: A DNS resolver.

   If $x is the IP address of a nameserver,
   and $n is a hostname, then

     R:lookup($x,$n)

   should return all the address records for $n.
:)

module namespace R = "resolver";
import module namespace S = "dxq:dns" at "dns.xqi";

declare variable $cache := <cache/>;

declare updating function local:lookup-cache($n){
  let $ans := 
    $cache/entry[a/@hostname=$n] 
  return
    if ($ans) then 
      for $a in $ans return (
       let $dif := $a/expires - glx:gettime() return
         if ( $dif > 0)
           then ( 
             let $adr := $a/a return (: now, update the TTL :)
              ( snap { replace value of node $adr/@ttl with $dif },
                $adr )      
         ) else (   (: expired entry :)
             snap { delete node $a } 
         )
    ) else ()
};

declare updating function local:update-cache($x, $r){
 insert node
   <entry>
      <server>{$x}</server>
      <expires>{glx:gettime() + data($r/@ttl)}</expires> 
      {$r}
   </entry> into $cache 
};


declare function R:lookup-no-cache($x,$n) {
 let server T implement S at $x return { 
  <rr>{ 
      from server T return { T:RR()/a[@hostname=$n] },
      for $ns in from server T return { T:RR()/ns }
      for $a in from server T return { T:RR()/a }

      where $ns/@nameserver=$a/@hostname
      and fn:not($ns/@domain=".")
      and glx:dns-lt($ns/@domain,$n)
      return
      R:lookup-no-cache($a/@address,$n)/a
  }</rr>
 }
};


declare function local:get-adr-lt($domain, $names) {
  for $n in $names return
    if (glx:dns-lt($domain,$n)) then $n
    else ()
};

declare function local:not-cached-addresses($names, $found){
  if (empty ($found)) then $names
  else
    for $n in $names return
      if (not ($n = $found/@hostname)) then $n else ()
};

(: Do a batch lookup inside the cache. :)

declare updating function local:batch-lookup-cache($names){
    for $n in $names
      for $e in $cache/entry
        where $n = $e/a/@hostname
      return
       let $dif := $e/expires - glx:gettime() return
         if ( $dif > 0)
           then ( 
             let $adr := $e/a return (: update the TTL :)
              ( snap { replace value of node $adr/@ttl with $dif },
                $adr )      
         ) else (   (: expired entry :)
             snap { delete node $e } 
         )         
};

(::::::::::::::::::::::::::::::::::::::::::::::::::)
(:                                                :)
(:                                                :)
(:                                                :)
(: public functions                               :)
(:                                                :)
(:                                                :)
(:                                                :)
(::::::::::::::::::::::::::::::::::::::::::::::::::)


(: The initial lookup function, but with caching. :) 

declare updating function R:lookup($x,$n) {
 let server T implement S at $x return { 
  <rr>{ 
      let $ad := local:lookup-cache($n) return
      if (empty ($ad)) then (
        let $ans := from server T { T:RR()/a[@hostname=$n] }
        return 
           if (not (empty($ans))) then
             (local:update-cache($x,$ans), $ans)
           else ()
        ,
        for $ns in from server T { S:RR()/ns }
        for $a in from server T { S:RR()/a }

        where $ns/@nameserver=$a/@hostname
        and fn:not($ns/@domain=".")
        and glx:dns-lt($ns/@domain,$n)
        return
        R:lookup($a/@address,$n)/a 
     ) else (
         $ad
     )
  }</rr>
 }
};


(: Does a batch lookup, looking first if some of the items are not in
   the local cache. :)

declare updating function R:batch-lookup($x,$names) {
 let server T implement S at $x return { 
  <rr>{ 
      let $addresses := local:batch-lookup-cache($names) 
      let $notcached := local:not-cached-addresses($names, $addresses)
      return
       ($addresses, 
        if (not (empty ($notcached))) then (
          let $ans := from server T return { T:RR()/a[@hostname=$notcached] }
          return
           if (not (empty($ans))) then (
              for $adr in $ans return
                (local:update-cache($x,$adr), $adr)
           ) else (),
          for $ns in from server T return { T:RR()/ns }
          for $a in from server T return { T:RR()/a }

          where $ns/@nameserver=$a/@hostname
          and fn:not($ns/@domain=".")
          return
            let $n2 := local:get-adr-lt($ns/@domain, $notcached) 
            return 
              if (not (empty ($n2))) then
                 R:batch-lookup($a/@address,$n2)/a  
              else ()
       ) else ())
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
      for $a in from server T return  { T:RR()/a }
      return $a 
    }
    </rr>
  }
};

declare function R:test-display-cache() {
  $cache
};

(: ----------------------------------------
     Use Case "Seq" : Operations on trees
   ---------------------------------------- :)

declare variable $report1 := doc("../docs/report1.xml");

declare function local:precedes($a as node(), $b as node()) as xs:boolean
{
     $a << $b
       and
     empty($a//node() intersect $b)
};

declare function local:follows($a as node(), $b as node()) as xs:boolean
{
     $a >> $b
       and
     empty($b//node() intersect $a)
};

declare function local:between($seq as node()*, $start as node(), $end as node()) as item()*
{
  let $nodes :=
    for $n in $seq except $start//node()
    where $n >> $start and $n << $end
    return $n
  return $nodes except $nodes//node()
};


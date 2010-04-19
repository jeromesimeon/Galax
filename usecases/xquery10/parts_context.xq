declare variable $partlist := doc("../docs/parts.xml");

declare function local:one_level($p as element()) as element()
{
     <part partid="{ $p/@partid }"
           name="{ $p/@name }" >
         {
             for $s in $partlist//part
             where $s/@partof = $p/@partid
             return local:one_level($s)
         }
     </part>
};



declare boundary-space preserve;

declare variable $lib := .;

declare variable $namemap := doc("/home/simeon/XQuery/APPLICATIONS/iTune/itmap.xml");

declare function local:mapname($ename) {
    if($ename castable as xs:integer)
    then
      ()
    else
      let $field := fn:string($namemap//key[. = $ename]/@name)
      return
        if ($field = "")
        then
          <map><name>{$ename}</name></map>
        else ()
};

for $k in distinct-values($lib//key)
return local:mapname($k)



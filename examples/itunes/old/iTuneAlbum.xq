declare boundary-space preserve;

declare variable $lib := .;

<albums>{ for $x in fn:distinct-values($lib//album)
    let $a :=
      for $y in $lib//track
      where $y/album = $x
      return $y
    order by $x
    return
      (text { "
  "},<album name="{$x}">{ for $x in $a 
      order by xs:integer($x/tnb)
      return (text { "
    "},<track tnb="{$x/tnb}">{$x/name,$x/artist,$x/time}</track>) }
  </album>) }
</albums>


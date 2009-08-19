declare boundary-space preserve;

declare variable $lib := .;

declare variable $namemap := doc("./itmap.xml");

declare function local:mapname($n) {
  fn:string($namemap//key[. = $n]/@name)
};

declare function local:mapkey($k,$kk) {
  let $ename := fn:string($k)
  let $econtent :=
    (typeswitch ($kk)
      case $d as element(dict) return local:map($d)
      case $d as element(array) return local:map($d/*)
      default return fn:string($kk))
  return
    if($ename castable as xs:integer)
    then element track { attribute id { $ename }, $econtent }
    else
      let $mname := local:mapname($ename) return
      if($mname = "")
      then fn:trace(("Missing key name",$ename),"[WARNING]")
      else element { $mname } { $econtent }
};

declare function local:map($d) {
  for $k in $d/key
  let $kk := $k/following-sibling::*[1]
  return
    local:mapkey($k,$kk)
};

<itune>{local:map(./plist/dict)}</itune>

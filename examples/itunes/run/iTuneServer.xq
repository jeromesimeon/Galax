module namespace iTuneServer = "iTuneServer";
import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";

declare boundary-space preserve;

declare variable $iTuneLib := doc("./gTunesMusicLibrary.xml");
declare variable $namemap := doc("./itmap.xml");
declare variable $albums := doc("./gTunesAlbums.xml");

declare function iTuneServer:getArtists() as xs:string* {
  for $x in fn:distinct-values($iTuneLib//artist)
  order by $x
  return $x
};

declare function iTuneServer:getAlbums() as xs:string* {
  for $x in fn:distinct-values($iTuneLib//album)
  order by $x
  return $x
};

declare function iTuneServer:make_albums() {
  <albums>{ for $x in fn:distinct-values($iTuneLib//album)
      let $a :=
        for $y in $iTuneLib//track
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
};

declare function iTuneServer:getAlbum($x as xs:string) {
  $albums//album[@name=$x]  
};

declare function iTuneServer:getTime($album as xs:string, $song as xs:string) as xs:integer {
  let $a := iTuneServer:getAlbum($album) return
  $a/track[name = $song]/time
};

declare function iTuneServer:computeDuration($x as xs:integer) as xs:duration {
  let $s := $x div 1000 return
  xs:duration(fn:concat("PT",$s,"S"))
};

declare updating function iTuneServer:addDuration($x as xs:integer, $album as xs:string, $song as xs:string) {
  let $a := iTuneServer:getAlbum($album) return
  let $s := $a/track[name = $song] return
  let $d := iTuneServer:computeDuration($s/time) return
  do insert <duration>{$d}</duration> as last into $s,
  dxq:broadcast($x)
};

declare updating function iTuneServer:f1() {
  let $x := dxq:cvcreate() return
  (dxq:wait($x,dxq:mcreate()), iTuneServer:getAlbum("Dirty Paper Cup"))
};

declare updating function iTuneServer:f2($x) {
  iTuneServer:addDuration(0,"Dirty Paper Cup","Younger Longer"),
  dxq:broadcast($x)
};

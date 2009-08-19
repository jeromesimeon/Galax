module namespace iTuneClient = "iTuneClient";
import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";
import interface namespace iTuneServer = "iTuneServer" at "./iTuneServer.xqi";

declare updating function iTuneClient:start() {
  let $x := dxq:cvcreate() return
  let server T implement iTuneServer at "iTuneServer" return
    (at server T do T:addDuration($x,"Dirty Paper Cup","Younger Longer"),
     (dxq:wait($x,dxq:mcreate()), T:getAlbum("Dirty Paper Cup")))
};

module namespace yoohoo = "urn:YooHoo";
declare namespace jbroster = "jabber:iq:roster";
declare namespace jbrdelay = "jabber:x:delay";

declare variable $users := ("jerome", "mary", "nicola");
declare variable $services := ("cell", "work", "im");

(: ---------------------------------------------------------------------- :)
(: Service accounts                                                       :)
(: For a given YooHoo user, get the services to which they are subscribed :)
(: ---------------------------------------------------------------------- :)
declare function yoohoo:user-services(  
  $userName as item()*
) as item()* {
  <user id="{$u}">{ 
    let $login := fn:concat($userName,"@localhost/tests"),
        $buddies := glx:jabber-buddies ($login, $userName,  xs:int("1"), fn:false())
    return 
      for $i in $buddies/response/jbroster:query/jbroster:item/@jid
      where fn:contains($i,$userName)
      return  <service id="{$i}"/>
  } </user>
};

(: ------------ :)
(: All accounts :)
(: ------------ :)
declare function yoohoo:accounts() as item()* { 
  <accounts> {
    for $u in $users return yoohoo:user-services($u)
  } </accounts>
};

(: ------------------------------------------------- :)
(: Service status                                    :)
(: For a given YooHoo user, get their service status :)
(: ------------------------------------------------- :)
declare function yoohoo:user-status(
  $userName as item()*
) as item()*
{ 
  <user id="{$userName}">{ 
    let $login := fn:concat($userName,"@localhost/tests"),
        $buddies := glx:jabber-buddies ($login, $userName,  xs:int("1"), fn:false())
    return $buddies/response/available/presence
  } </user>
};

declare function yoohoo:status() as item()* { 
  <status> {
    for $u in $users return yoohoo:user-status($u)
  } </status>
};

(: -------------------------------------------------------------- :)
(: YooHoo presence                                                :)
(: For a given YooHoo user, report their status on their "work",  :)
(: "cell" and "im" services                                       :)
(: -------------------------------------------------------------- :)

declare function yoohoo:presence(  
  $userName as item()*
) as item()* { 
  let $status := yoohoo:user-status($userName)
  return 
    <user id="{ $userName }"> { 
      if ($status/presence) then 
        for $svc in $services 
        let $svcstatus := $status/presence[fn:contains(@from,$svc)]/status
        return element { $svc } { $svcstatus }
      else <status>No Services Available</status>
    }</user>
};

(: ------------------------------------------------- :)
(: Jabber buddies                                    :)
(: For a given YooHoo user, dump their Jabber status :)
(: ------------------------------------------------- :)
declare function yoohoo:dump-buddies(
  $userName as item()*
) as item()*
{ 
  let $login := fn:concat($userName,"@localhost/tests"),
      $buddies := glx:jabber-buddies ($login, $userName,  xs:int("1"), fn:false())
  return $buddies/response
};


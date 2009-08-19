declare namespace yoohoo ="yoohoo";
declare namespace jbroster = "jabber:iq:roster";
declare namespace jbrdelay = "jabber:x:delay";
declare variable $thisuser { "jerome@localhost/tests" };
declare variable $thispassword { "jerome" };

declare function yoohoo:presence(  
  $userName as item()*,
  $buddyName as item()*
) as item()* { 
  let $buddies := glx:jabber-buddies ($thisuser, $thispassword, xs:int("1"), fn:false()),
      $status :=  $buddies/response/available/presence/jbrdelay:x[fn:contains(@from,$buddyName)]
  return
    if ($status) then $status
    else if ($buddies/response/jbroster:query/jbroster:item[@jid=$buddyName]) then <status>Offline</status>
    else  <status>Not in my roster</status>
};


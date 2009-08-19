declare variable $count as xs:integer := 0;
declare updating function local:prune($d as xs:date) as xs:integer 
{
   for $m in doc("messages.xml")/mail/message[date < $d]
   return
     { delete node $m; set $count := $count + 1; () }, 
   $count 
};

local:prune("2006-08-01")

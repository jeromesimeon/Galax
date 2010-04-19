(: Q1: Synchronize the three logs. :)
(: Q1: Synchronize the three logs. :)
for $a in doc("../docs/archive.xml")/archived-agenda/entry,
   $v1 in doc("../docs/copy1.xml")/agenda-version/entry,
   $v2 in doc("../docs/copy2.xml")/agenda-version/entry
where $a/name = $v1/name
 and $v1/name = $v2/name
return
 if ($a/contact = $v1/contact and $v1/contact=$v2/contact)
 then ()
 else
   if ($v1/contact = $v2/contact)
   then replace value of node $a/contact with $v1/contact
   else
     if ($a/contact = $v1/contact)
     then (
           replace value of node $a/contact with $v2/contact,
           replace value of node $v1/contact with $v2/contact
           )
     else
       if ($a/contact = $v2/contact)
       then (
             replace value of node $a/contact with $v1/contact,
             replace value of node $v2/contact with $v1/contact
            )
       else (
         insert node
           <fail>
              <arch>{ $a }</arch>
              <v1>{ $v1 }</v1>
              <v2>{ $v2 }</v2>
           </fail>
         into doc("../docs/log.xml")/log
       )
,
replace value of node doc("../docs/archive.xml")/*/last-synch-time
       with xs:dateTime("2007-08-17T16:45:02-05:00")(: current-dateTime() :)
;
(: Q2: log.xml. :)
(: Q2: log.xml. :)
doc("../docs/log.xml");
(: Q3: log.xml. :)
(: Q3: log.xml. :)
doc("../docs/archive.xml")

(: Q4: Add a radio to the car. :)
(: Q4: Add a radio to the car in "part-tree.xml", using a part number that hasn't been taken. :)
let $next := max(doc("docs/part-tree.xml")//@partid) + 1
  return
    insert node <part partid="{$next}" name="radio"/>
       as last into 
       doc("docs/part-tree.xml")//part[@partid=0 and @name="car"]
;
doc("docs/part-tree.xml")

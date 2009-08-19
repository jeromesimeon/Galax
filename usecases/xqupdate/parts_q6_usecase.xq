(: Q6: Renumber inventory items :)
(: Q6:  The head office has adopted a new numbering scheme. 
        In "part-tree.xml", add 1000 to all part numbers for 
        cars, 2000 to all part numbers for skateboards, and 
        3000 to all part numbers for canoes. :)
let $next := max(doc("docs/part-tree.xml")//@partid) + 1
  return
    insert node <part partid="{$next}" name="radio"/>
       as last into 
       doc("docs/part-tree.xml")//part[@partid=0 and @name="car"]
;

for $keyword at $i in ("car", "skateboard", "canoe"),
    $parent in doc("docs/part-tree.xml")//part[@name=$keyword]
let $descendants := $parent//part
for $p in ($parent, $descendants)
return 
  replace value of node $p/@partid with $i*1000+$p/@partid
;
doc("docs/part-tree.xml")


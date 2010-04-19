(: Q3: Delete all parts. :)
(: Q3: Delete all parts belonging to a car in "part-list.xml", leaving the car itself. :)
(: Solution 1 in the XQuery Update Facility (leveraging "part-tree.xml"): :)

for $pt in doc("../docs/part-tree.xml")//part[@name="car"]//part, 
    $pl in doc("../docs/part-list.xml")//part
where $pt/@partid eq $pl/@partid
return
  delete nodes $pl 
;
doc("../docs/part-list.xml")


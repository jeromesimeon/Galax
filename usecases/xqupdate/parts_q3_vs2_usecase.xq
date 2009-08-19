(: Q3: Delete all parts. :)
(: Q3: Delete all parts belonging to a car in "part-list.xml", leaving the car itself. :)
(: Solution 2 (using a recursive updating function): :)

declare updating function 
             local:delete-subtree($p as element(part))
  {
      for $child in doc("docs/part-list.xml")//part
      where $p/@partid eq $child/@partof
      return (
        delete nodes $child,
        local:delete-subtree($child)
      )
  };

for $p in doc("docs/part-tree.xml")//part[@name="car"]
return 
  local:delete-subtree($p) 
;
doc("docs/part-list.xml")

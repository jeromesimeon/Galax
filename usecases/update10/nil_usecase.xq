(: Q1: Maintaining schema validity :)
(: Q1: This use case demonstrates transform expressions which construct modified copies of some data, which must remain valid according to the original schema. In this use case, keeping the modified copy valid requires adding an xsi:nil attribute. :)
for $e in doc("../docs/employees.xml")//employee
where $e/@mgr = true()
return
  copy $emp := $e
  modify (
      replace value of node $emp/salary with "" , 
      insert node (attribute xsi:nil {"true"}) 
      into $emp/salary
  )
  return $emp

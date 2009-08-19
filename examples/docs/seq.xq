import module namespace seq_context = "seq_context" at "../docs/seq_context.xq"; 
declare variable $x external; 
declare variable $y external; 
for $s in $seq_context:report//section[section.title = "Procedure"] return ($s//incision)[2]/instrument 

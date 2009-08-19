{ declare $y := <holiday/>;
  set $y := <holiday>{xs:date("2006-07-04")}</holiday>;
  $y
}
,
{ declare $total-cost as xs:decimal := 0;
  for $p in doc("docs/project.xml")/projects/project[year = 2005]
  return
    {  set $total-cost := $total-cost + $p/cost;
       <project>
         <name>{$p/name}</name>
         <cost>{$p/cost}</cost>
         <cumulative-cost>{$total-cost}</cumulative-cost>
       </project>
     }
}



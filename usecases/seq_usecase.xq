(: Q1: Second instruments after incision :)
(: Q1: In the Procedure section of Report1, what Instruments were used in the second Incision?  :)
for $s in $report1//section[section.title = "Procedure"]
return ($s//incision)[2]/instrument

;
(: Q2: First 2 instruments used :)
(: Q2: In the Procedure section of Report1, what are the first two Instruments to be used?   :)

for $s in $report1//section[section.title = "Procedure"]
return ($s//instrument)[position()<=2]

;
(: Q3: Instruments after 2nd incision? :)
(: Q3:  In Report1, what Instruments were used in the first two Actions after the second Incision?  :)
let $i2 := ($report1//incision)[2] 
for $a in ($report1//action)[. >> $i2][position()<=2]
return $a//instrument

;
(: Q4: No anesthesia? :)
(: Q4: In Report1, find "Procedure" sections where no Anesthesia element occurs before the first Incision. :)
for $p in $report1//section[section.title = "Procedure"]
where not(some $a in $p//anesthesia satisfies 
          $a << ($p//incision)[1])
return $p

;
(: Q5: Between 1st & 2nd incision :)
(: Q5: In Report1, what happened between the first Incision and the second Incision? :)
<critical_sequence>
  {
   let $proc := $report1//section[section.title="Procedure"][1]
   for $n in $proc//node()
   where local:follows($n, ($proc//incision)[1])
     and local:precedes($n, ($proc//incision)[2])
   return $n
  }
</critical_sequence>


;
(: Q5a: An alternative to Q5 :)
(: Q5a: In Report1, what happened between the first Incision and the second Incision? :)

<critical_sequence>
{
  let $proc := $report1//section[section.title="Procedure"][1],
      $i1 :=  ($proc//incision)[1],
      $i2 :=  ($proc//incision)[2]
  for $n in $proc//node() except $i1//node()
  where $n >> $i1 and $n << $i2
  return $n 
 }
</critical_sequence> 

;
(: Q5b: Another alternative to Q5 :)
(: Q5b: In Report1, what happened between the first Incision and the second Incision? :)

<critical_sequence>
 {
  let $proc := $report1//section[section.title="Procedure"][1],
      $first :=  ($proc//incision)[1],
      $second:=  ($proc//incision)[2]
  return local:between($proc//node(), $first, $second)
 }
</critical_sequence>

            


;

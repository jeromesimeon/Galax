(: Q1: Selection. :)
(: Q1: Return the name of the person with ID `person0'  registered in North America. :)

for    $b in $auction/site/people/person[@id="person0"]
return $b/name/text()


;
(: Q2: Element construction :)
(: Q2: Return the initial increases of all open auctions. :)

for $b in $auction/site/open_auctions/open_auction
return <increase> { $b/bidder[1]/increase/text() } </increase>


;
(: Q3: Complex selection :)
(: Q3. Return the IDs of all open auctions whose current increase is at least twice as high as the initial increase. :)

for    $b in $auction/site/open_auctions/open_auction
where  $b/bidder[1]/increase/text() * 2.0 <= $b/bidder[last()]/increase/text()
return <increase first="{ $b/bidder[1]/increase/text() }"
                 last="{ $b/bidder[last()]/increase/text() }"/>


;
(: Q4. Document order :)
(: Q4. List the reserves of those open auctions where a certain person issued a bid before another person. :)

for    $b in $auction/site/open_auctions/open_auction
where  some $pr1 in $b/bidder/personref[@person="person20"],
            $pr2 in $b/bidder/personref[@person="person51"] satisfies
       $pr1 << $pr2
return <history> { $b/reserve/text() } </history>


;
(: Q5: Aggregation :)
(: Q5: How many sold items cost more than 40? :)

count(for $i in $auction/site/closed_auctions/closed_auction
      where  $i/price/text() >= 40.0
      return $i/price)


;
(: Q6: More aggregation :)
(: Q6: How many items are listed on all continents? :)

for    $b in $auction//site/regions
return count($b//item)


;
(: Q7: Aggregation and arithmetics :)
(: Q7: How many pieces of prose are in our database? :)

for $p in $auction/site
return count($p//description) + count($p//annotation) + count($p//emailaddress)


;
(: Q8: Join :)
(: Q8: List the names of persons and the number of items they bought. (joins person, closed_auction) :)

for $p in $auction/site/people/person
let $a := for $t in $auction/site/closed_auctions/closed_auction
             where $t/buyer/@person = $p/@id
             return $t
return <item person="{ $p/name/text() }"> { count($a) } </item>


;
(: Q9: More join :)
(: Q9: List the names of persons and the names of the items they bought in Europe.  (joins person, closed\_auction, item)} :)

for $p in $auction/site/people/person
let $a :=
  for $t in $auction/site/closed_auctions/closed_auction
  where $p/@id = $t/buyer/@person
  return
  let $n :=
    for $t2 in $auction/site/regions/europe/item
    where $t/itemref/@item = $t2/@id
    return $t2
  return <item>{$n/name/text()}</item>
return <person name="{$p/name/text()}">{$a}</person>


;
(: Q10: Grouping :)
(: Q10: List all persons according to their interest; use French markup in the result. :)

for $i in distinct-values($auction/site/people/person/profile/interest/@category)
let $p := for    $t in $auction/site/people/person
          where  $t/profile/interest/@category = $i
            return
              <personne>
                <statistiques>
                  <sexe> { $t/profile/gender/text() } </sexe>
                  <age> { $t/profile/age/text() } </age>
                  <education> { $t/profile/education/text() } </education>
                  <revenu> { fn:data($t/profile/@income) } </revenu>
                </statistiques>
                <coordonnees>
                  <nom> { $t/name/text() } </nom>
                  <rue> { $t/address/street/text() } </rue>
                  <ville> { $t/address/city/text() } </ville>
                  <pays> { $t/address/country/text() } </pays>
                  <reseau>
                    <courrier> { $t/emailaddress/text() } </courrier>
                    <pagePerso> { $t/homepage/text() } </pagePerso>
                  </reseau>
                </coordonnees>
                <cartePaiement> { $t/creditcard/text() } </cartePaiement>     
              </personne>
return <categorie>
         <id> { $i } </id>
         { $p }
       </categorie>


;
(: Q11: More group by :)
(: Q11: For each person, list the number of items currently on sale whose price does not exceed 0.02% of the person's income. :)

for $p in $auction/site/people/person
let $l := for $i in $auction/site/open_auctions/open_auction/initial
          where $p/profile/@income > (5000.0 * $i/text())
          return $i
return <items name="{ $p/name/text() }"> { count($l) } </items>


;
(: Q12:  More more group by :)
(: Q12:  For each richer-than-average person, list the number of items  currently on sale whose price does not exceed 0.02% of the person's income. :)

for $p in $auction/site/people/person
let $l := for $i in $auction/site/open_auctions/open_auction/initial
          where $p/profile/@income > (5000.0 * $i/text())
          return $i
where  $p/profile/@income > 50000.0
return <items person="{ $p/profile/@income }"> { count($l) } </items>


;
(: Q13: Long path expression :)
(: Q13: List the names of items registered in Australia along with their descriptions. :)

for $i in $auction/site/regions/australia/item
return <item name="{ $i/name/text() }"> { $i/description } </item>


;
(: Q14: String operation :)
(: Q14. Return the names of all items whose description contains the word `gold'. :)

for $i in $auction/site//item
where contains(string($i/description),"gold")
return $i/name/text()

;
(: Q15: Loooooong path expression :)
(: Q15: Print the keywords in emphasis in annotations of closed auctions. :)

for $a in $auction/site/closed_auctions/closed_auction/annotation/description/parlist/listitem/parlist/listitem/text/emph/keyword/text()
return <text> { $a } </text>


;
(: Q16: Moooore loong path expression :)
(: Q16: Return the IDs of those auctions that have one or more keywords in emphasis. (cf. Q15) :)

for $a in $auction/site/closed_auctions/closed_auction
where not(empty($a/annotation/description/parlist/listitem/parlist/listitem/text/emph/keyword/text()))

return <person id="{ $a/seller/@person }"/>


;
(: Q17: Emptiness :)
(: Q17: Which persons don't have a homepage? :)

for    $p in $auction/site/people/person
where  empty($p/homepage/text())
return <person name="{ $p/name/text() }"/>


;
(: Q18: Function call :)
(: Q18: Convert the currency of the reserve of all open auctions to another currency. :)

for    $i in $auction/site/open_auctions/open_auction
return local:convert($i/reserve)


;
(: Q19: Sort by :)
(: Q19. Give an alphabetically ordered list of all items along with their location. :)

for    $b in $auction/site/regions//item
let    $k := $b/name/text()
order by $b/location
return <item name="{ $k }"> { $b/location/text() } </item>

;
(: Q20: Back to group by  :)
(: Q20: Group customers by their income and output the cardinality of each group. :)

<result>
 <preferred>
   { count($auction/site/people/person/profile[@income >= 100000.0]) }
 </preferred>
 <standard>
   { count($auction/site/people/person/profile[@income < 100000.0
                                              and @income >= 30000.0]) }
 </standard>
 <challenge> 
   { count($auction/site/people/person/profile[@income < 30000.0]) }
 </challenge>
 <na>
   { count(for    $p in $auction/site/people/person
           where  empty($p/profile/@income)
           return $p) }
 </na>
</result>


;

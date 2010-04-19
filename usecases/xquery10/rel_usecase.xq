(: Q1: Selection query :)
(: Q1: List the item number and description of all bicycles that currently have an auction in progress, ordered by item number. Note - The solution assumes that the current date is 1999-01-31 :)

<result>
  {
    for $i in $items//item_tuple
    where $i/start_date <= xs:date("1999-01-31") (: current-date() :)
      and $i/end_date >= xs:date("1999-01-31")   (: current-date() :)
      and contains($i/description, "Bicycle")
    order by $i/itemno
    return
        <item_tuple>
            { $i/itemno }
            { $i/description }
        </item_tuple>
  }
</result>

;
(: Q2: Join and sorting  :)
(: Q2: List the item number and description of all bicycles that currently have an auction in progress, ordered by item number. :)
<result>
{
  for $i in $items//item_tuple
  let $b := $bids//bid_tuple[itemno = $i/itemno]
  where contains($i/description, "Bicycle")
  order by $i/itemno 
  return
    <item_tuple>
      { $i/itemno }
      { $i/description }
     <high_bid>{ max($b/bid) }</high_bid>
    </item_tuple>
}
</result> 

;
(: Q3: Complex join  :)
(: Q3: Find cases where a user with a rating worse (alphabetically, greater) than "C" is offering an item with a reserve price of more  than 1000. :)
<result>
   {
     for $u in $users//user_tuple
     for $i in $items//item_tuple
     where $u/rating > "C"
        and $i/reserve_price > 1000
        and $i/offered_by = $u/userid
     return
         <warning>
             { $u/name }
             { $u/rating }
             { $i/description }
             { $i/reserve_price }
         </warning>
   }
</result>

;
(: Q4: Emptiness checking  :)
(: Q4: List item numbers and descriptions of items that have no bids. :)
<result>
   {
     for $i in $items//item_tuple
     where empty($bids//bid_tuple[itemno = $i/itemno])
     return
         <no_bid_item>
             { $i/itemno }
             { $i/description }
         </no_bid_item>
   }
</result>

;
(: Q5: Multiway join!  :)
(: Q5: For bicycle(s) offered by Tom Jones, list the item number, description, highest bid (if any), and name of the highest bidder, ordered by item number. :)
<result>
   {
     for $seller in $users//user_tuple,
         $buyer in  $users//user_tuple,
         $item in  $items//item_tuple,
         $highbid in  $bids//bid_tuple
     where $seller/name = "Tom Jones"
       and $seller/userid  = $item/offered_by
       and contains($item/description , "Bicycle")
       and $item/itemno  = $highbid/itemno
       and $highbid/userid  = $buyer/userid
       and $highbid/bid = max($bids//bid_tuple[itemno = $item/itemno]/bid)
     order by ($item/itemno)
     return
         <jones_bike>
             { $item/itemno }
             { $item/description }
             <high_bid>{ $highbid/bid }</high_bid>
             <high_bidder>{ $buyer/name }</high_bidder>
         </jones_bike>
   }
</result>


;
(: Q6: Some arithmetics.  :)
(: Q6: For each item whose highest bid is more than twice its reserve price, list the item number, description, reserve price, and highest bid. :)
<result>
   {
     for $item in $items//item_tuple
     let $b := $bids//bid_tuple[itemno = $item/itemno]
     let $z := max($b/bid)
     where $item/reserve_price * 2 < $z
     return
         <successful_item>
             { $item/itemno }
             { $item/description }
             { $item/reserve_price }
             <high_bid>{$z }</high_bid>
          </successful_item>
   }
</result>


;
(: Q7: Aggregate & Full-text operators :)
(: Q7: Find the highest bid ever made for a bicycle or tricycle. :)
let $allbikes := $items//item_tuple
                     [contains(description, "Bicycle")
                      or contains(description, "Tricycle")]
let $bikebids := $bids//bid_tuple[itemno = $allbikes/itemno]
return
     <high_bid>
       {
         max($bikebids/bid)
       }
     </high_bid>


;
(: Q8: Count items :)
(: Q8: How many items were actioned (auction ended) in March 1999? :)
let $item := $items//item_tuple
  [end_date >= xs:date("1999-03-01") and end_date <= xs:date("1999-03-31")]
return
    <item_count>
      { 
        count($item) 
      }
    </item_count>
;
(: Q9: Group and order :)
(: Q9: List the number of items auctioned each month in 1999 for which data is available, ordered by month :)
<result>
  {
    let $end_dates := $items//item_tuple/end_date
    for $m in distinct-values(for $e in $end_dates 
                              return month-from-date($e))
    let $item := $items
        //item_tuple[year-from-date(end_date) = 1999 
                     and month-from-date(end_date) = $m]
    order by $m
    return
        <monthly_result>
            <month>{ $m }</month>
            <item_count>{ count($item) }</item_count>
        </monthly_result>
  }
</result>
;
(: Q10: Re-grouping :)
(: Q10: For each item that has received a bid, list the item number, the highest bid, and the name of the highest bidder, ordered by item number. :)
<result>
  {
     for $highbid in $bids//bid_tuple,
         $user in $users//user_tuple
     where $user/userid = $highbid/userid
       and $highbid/bid = max($bids//bid_tuple[itemno=$highbid/itemno]/bid)
     order by $highbid/itemno
     return
         <high_bid>
             { $highbid/itemno }
             { $highbid/bid }
             <bidder>{ $user/name/text() }</bidder>
         </high_bid>
   }
</result>

;
(: Q11: Path expressions, aggregates :)
(: Q11: List the item number and description of the item(s) that received the highest bid ever recorded, and the amount of that bid. :)
let $highbid := max($bids//bid_tuple/bid)
return
     <result>
      {
         for $item in $items//item_tuple,
             $b in $bids//bid_tuple[itemno = $item/itemno]
         where $b/bid = $highbid
         return
             <expensive_item>
                 { $item/itemno }
                 { $item/description }
                 <high_bid>{ $highbid }</high_bid>
             </expensive_item>
      }
     </result>


;
(: Q12: List "popular" items :)
(: Q12: List the item number and description of the item(s) that received the largest number of bids, and the number of bids it (or they) received. :)
<result>
  {
     let $bid_counts := local:bid_summary(),
         $maxbids := max($bid_counts/nbids),
         $maxitemnos := $bid_counts[nbids = $maxbids]
     for $item in $items//item_tuple,
         $bc in $bid_counts
     where $bc/nbids =  $maxbids and $item/itemno = $bc/itemno
     return
         <popular_item>
             { $item/itemno }
             { $item/description }
             <bid_count>{ $bc/nbids/text() }</bid_count>
         </popular_item>
  }
</result>

;
(: Q13: Regroup by bidding user :)
(: Q13: For each user who has placed a bid, give the userid, name, number of bids, and average bid, in order by userid. :)
<result>
  {
     for $uid in distinct-values($bids//userid),
         $u in $users//user_tuple[userid = $uid]
     let $b := $bids//bid_tuple[userid = $uid]
     order by $u/userid
     return
         <bidder>
             { $u/userid }
             { $u/name }
             <bidcount>{ count($b) }</bidcount>
             <avgbid>{ avg($b/bid) }</avgbid>
         </bidder>
   }
</result>

;
(: Q14: List items with average bids :)
(: Q14: List item numbers and average bids for items that have received three or more bids, in descending order by average bid. :)
<result>
  {
     for $i in distinct-values($bids//itemno)
     let $b := $bids//bid_tuple[itemno = $i]
     let $avgbid := avg($b/bid)
     where count($b) >= 3
     order by $avgbid descending
       return
         <popular_item>
             <itemno>{ $i }</itemno>
             <avgbid>{ $avgbid }</avgbid>
         </popular_item>
   }
</result>

;
(: Q15: List users with multiple bids :)
(: Q15: List names of users who have placed multiple bids of at least $100 each. :)
<result>
   {
     for $u in $users//user_tuple
     let $b := $bids//bid_tuple[userid=$u/userid and bid>=100]
     where count($b) > 1
     return
         <big_spender>{ $u/name/text() }</big_spender>
   }
</result>

;
(: Q16: Lists users :)
(: Q16: List all registered users in order by userid; for each user, include the userid, name, and an indication of whether the user is active (has at least one bid on record) or inactive (has no bid on record). :)
<result>
   {
     for $u in $users//user_tuple
     let $b := $bids//bid_tuple[userid = $u/userid]
     order by $u/userid
     return
         <user>
             { $u/userid }
             { $u/name }
             {
                 if (empty($b))
                 then <status>inactive</status>
                 else <status>active</status>
             }
         </user>
   }
</result>

;
(: Q17: List "very active" bidders :)
(: Q17: List the names of users, if any, who have bid on every item. :)
<frequent_bidder>
   {
     for $u in $users//user_tuple
     where
       every $item in $items//item_tuple satisfies
         some $b in $bids//bid_tuple satisfies
           ($item/itemno = $b/itemno and $u/userid = $b/userid)
     return
         $u/name
   }
</frequent_bidder>

;
(: Q18: List bidders alphabetically :)
(: Q18: List all users in alphabetic order by name. For each user, include descriptions of all the items (if any) that were bid on by that user, in alphabetic order. :)
<result>
{
    for $u in $users//user_tuple
    order by $u/name
    return
        <user>
            { $u/name }
            {
                for $b in distinct-values($bids//bid_tuple
                                             [userid = $u/userid]/itemno)
                for $i in $items//item_tuple[itemno = $b]
                let $descr := $i/description/text()
                order by $descr
                return
                    <bid_on_item>{ $descr }</bid_on_item>
            }
        </user>
}
</result>

;

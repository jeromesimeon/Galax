declare boundary-space preserve;
(: Q1: Add new user. :)
(: Q1: Add a new user (with no rating) to the users.xml view. :)
insert node
  <user_tuple>
    <userid>U07</userid>
    <name>Annabel Lee</name>
  </user_tuple>
into doc("../docs/users.xml")/users
;
doc("../docs/users.xml")
;
(: Q2: Enter a bid. :)
(: Q2: Enter a bid for user Annabel Lee on February 1st, 1999 for 60 dollars on item 1001. :)
let $uid := 
doc("../docs/users.xml")/users/user_tuple[name="Annabel Lee"]/userid
return
  insert node
    <bid_tuple> 
      <userid>{data($uid)}</userid> 
      <itemno>1001</itemno> 
      <bid>60</bid> 
      <bid_date>1999-02-01</bid_date> 
    </bid_tuple>
  into doc("../docs/bids.xml")/bids 
;
doc("../docs/bids.xml") 
;
(: Q3: Insert a new bid. :)
(: Q3: Insert a new bid for Annabel Lee on item 1002, adding 10% to the best bid received so far for this item. :)
let $uid := 
doc("../docs/users.xml")/users/user_tuple[name="Annabel Lee"]/userid
let $topbid := 
max(doc("../docs/bids.xml")/bids/bid_tuple[itemno=1002]/bid)
return 
  insert node
    <bid_tuple> 
      <userid>{data($uid)}</userid> 
      <itemno>1002</itemno> 
      <bid>{$topbid*1.1}</bid> 
      <bid_date>1999-02-01</bid_date> 
    </bid_tuple>
  into doc("../docs/bids.xml")/bids
;
doc("../docs/bids.xml")
;
(: Q4: Set rating. :)
(: Q4: Set Annabel Lee's rating to B. :)
let $user := doc("../docs/users.xml")/users/user_tuple[name="Annabel Lee"] return 
  if ($user/rating)     
    then replace value of node $user/rating with "B" 
    else insert node <rating>B</rating> into $user
;
doc("../docs/users.xml")
;
(: Q5: Place a bid. :)
(: Q5:
Place a bid for Annabel Lee on item 1007, adding 10% to the best bid received so far on that item, but only if the bid amount does not exceed a given limit. The first query illustrates the desired behavior if the limit is exceeded.
:)
let $uid := 
doc("../docs/users.xml")/users/user_tuple[name="Annabel Lee"]/userid
let $topbid := 
max(doc("../docs/bids.xml")/bids/bid_tuple[itemno=1007]/bid)
where $topbid*1.1 <= 200
return 
  insert node 
    <bid_tuple> 
      <userid>{data($uid)}</userid> 
      <itemno>1007</itemno> 
      <bid>{$topbid*1.1}</bid> 
      <bid_date>1999-02-01</bid_date> 
    </bid_tuple>
  into doc("../docs/bids.xml")/bids
;
doc("../docs/bids.xml")
;
(: Q5a: Place a bid (alternate). :)
(: Q5a:
In the above, adding 10% to the best bid on item 1007 would have required a bid of 237, which is more than the allowed limit of 200. Thus, the bids.xml document does not change.

Place a bid for Annabel Lee on item 1007, adding 10% to the best bid received so far on that item, but only if the bid amount does not exceed 500. This illustrates the behavior when the resulting value is within the limit.
:)
let $uid := 
doc("../docs/users.xml")/users/user_tuple[name="Annabel Lee"]/userid
let $topbid := 
max(doc("../docs/bids.xml")/bids/bid_tuple[itemno=1007]/bid)
where $topbid*1.1 <= 500
return 
  insert node 
    <bid_tuple> 
      <userid>{data($uid)}</userid> 
      <itemno>1007</itemno> 
      <bid>{$topbid*1.1}</bid> 
      <bid_date>1999-02-01</bid_date> 
    </bid_tuple>
  into doc("../docs/bids.xml")/bids
;
doc("../docs/bids.xml")
;
(: Q6: Erase user. :)
(: Q6: Erase user Dee Linquent and the corresponding associated items and bids. :)
let $user := 
doc("../docs/users.xml")/users/user_tuple[name="Dee Linquent"]
let $items := 
doc("../docs/items.xml")/items/item_tuple[offered_by=$user/userid]
let $bids := 
doc("../docs/bids.xml")/bids/bid_tuple[userid=$user/userid]
return (
  delete nodes $user,
  delete nodes $items,
  delete nodes $bids
)
(:

An alternative solution is:

let $user := 
doc("users.xml")/users/user_tuple[name="Dee Linquent"]
let $items := 
doc("items.xml")/items/item_tuple[offered_by=$user/userid]
let $bids := 
doc("bids.xml")/bids/bid_tuple[userid=$user/userid]
return 
  delete nodes ($user, $items, $bids)

:)
;
doc("../docs/items.xml");
doc("../docs/users.xml");
doc("../docs/bids.xml")
;
(: Q7:  Add a <comment/>. :)
(: Q7:  Add the element <comment>This is a bargain !</comment> as the last child of the <item> element describing item 1002. :)
insert node
  <comment>This is a bargain !</comment>
as last into doc("../docs/items.xml")/items/item_tuple[itemno=1002] 
;
doc("../docs/items.xml")
;
(: Q8: Place a bid. :)
(: Q8: Place a bid for Annabel Lee on item 1010, which does not exist in "items.xml". An application constraint requires that no bid be placed on an item which was not previously registered in the database. :)
let $uid := 
doc("../docs/users.xml")/users/user_tuple[name="Annabel Lee"]/userid
return
  insert node
    <bid_tuple> 
      <userid>{data($uid)}</userid> 
      <itemno>1010</itemno> 
      <bid>60</bid> 
      <bid_date>2006-04-23</bid_date> 
    </bid_tuple>
  into doc("../docs/bids.xml")/bids 
;
doc("../docs/bids.xml")
;
(: Q9: Add a bid. :)
(: Q9: Add a bid for Annabel Lee on item 1002, at a price 5 dollars below the current highest bid. An auction application constraint imposes that a bid cannot be made at a lower price than the highest bid made so far on that item. :)
let $uid := 
doc("../docs/users.xml")/users/user_tuple[name="Annabel Lee"]/userid
let $topbid := 
max(doc("../docs/bids.xml")//bid_tuple[itemno=1002]/bid)
return
  insert node
    <bid_tuple> 
      <userid>{data($uid)}</userid> 
      <itemno>1002</itemno> 
      <bid>{$topbid - 5.00}</bid> 
      <bid_date>2006-04-23</bid_date> 
    </bid_tuple>
  into doc("../docs/bids.xml")/bids 
;
doc("../docs/bids.xml")

(: Q1: List unique namespaces. :)
(: Q1: List all unique namespaces used in the sample data. :)
<Q1>
   {
     for $n in distinct-values(
                   for $i in ($auction//* | $auction//@*)
                   return namespace-uri($i)
                )
     return  <ns>{$n}</ns>
   }
</Q1>

;
(: Q2: Select titles in music namespace :)
(: Q2: Select the title of each record that is for sale. :)
<Q2>
   {
     $auction//music:title
   }
</Q2>

;
(: Q3: Select all attributes in dt namespace :)
(: Q3: Select all attributes using datatypes from "XML Schema: Part 2" datatypes. :)
<Q3>
   {
     $auction//*[@dt:*]
   }
</Q3>

;
(: Q4: List target URI's of xlinks :)
(: Q4: List the target URI's of all XLinks in the document. :)
<Q4 xmlns:xlink="http://www.w3.org/1999/xlink">
   {
     for $hr in $auction//@xlink:href
     return <ns>{ $hr }</ns>
   }
</Q4>

;
(: Q5: Select on xml:lang attribute :)
(: Q5: Select all records that have a remark in German. :)
<Q5 xmlns:music="http://www.example.org/music/records">
   {
      $auction//music:record[music:remark/@xml:lang = "de"]
   }
</Q5>

;
(: Q6: Select on existence of attribute :)
(: Q6: Select the closing time elements of all AnyZone auctions currently monitored. :)
<Q6 xmlns:ma="http://www.example.com/AuctionWatch">
   {
     $auction//ma:Auction[@anyzone:ID]/ma:Schedule/ma:Close
   }
</Q6>

;
(: Q7: Query namespace URI :)
(: Q7: Select the homepage of all auctions where both seller and high bidder are registered at the same auctioneer. :)
<Q7 xmlns:xlink="http://www.w3.org/1999/xlink">
  {
    for $a in $auction//ma:Auction
    let $seller_id := $a/ma:Trading_Partners/ma:Seller/*:ID,
        $buyer_id := $a/ma:Trading_Partners/ma:High_Bidder/*:ID
    where namespace-uri($seller_id) = namespace-uri($buyer_id)
    return
        $a/ma:AuctionHomepage
  }
</Q7> 
;
(: Q8: Query namespace URI :)
(: Q8: Select all traders (either seller or high bidder) without negative comments :)
<Q8 xmlns:ma="http://www.example.com/AuctionWatch"
    xmlns:eachbay="http://www.example.com/auctioneers#eachbay" 
    xmlns:xlink="http://www.w3.org/1999/xlink">
  {
    for $s in $auction//ma:Trading_Partners/(ma:Seller | ma:High_Bidder)
(: Bug in usecase --  where $s/*:NegativeComments = 0
   Should be
:)
    where empty($s/*:NegativeComments)
    return $s
  }
</Q8>
;

(: ------------------------------------------
    Use Case "R" : Access to Relational Data
   ------------------------------------------ :)

declare variable $users := doc("docs/users.xml");
declare variable $items := doc("docs/items.xml");
declare variable $bids  := doc("docs/bids.xml");

declare function local:bid_summary() as element()*
{
     for $i in distinct-values($bids//itemno)
     let $b := $bids//bid_tuple[itemno = $i]
     return
         <bid_count>
             <itemno>{ $i }</itemno>
             <nbids>{ count($b) }</nbids>
         </bid_count>
};



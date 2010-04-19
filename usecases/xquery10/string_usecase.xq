(: Q1: Match string in new items. :)
(: Q1: Find all news items where the string "Foobar Corporation" appears in the title. :)
$input//news_item/title[contains(., "Foobar Corporation")] 

;
(: Q2: Match string in same title/paragraph. :)
(: Q2: Find news items where the Foobar Corporation and one or more of its partners are mentioned in the same paragraph and/or title. List each news item by its title and date.  :)

let $foobar_partners := local:partners("Foobar Corporation")
for $item in $input//news_item
where
  some $t in $item//title satisfies
    (contains($t/text(), "Foobar Corporation")
    and some $partner in $foobar_partners satisfies
      contains($t/text(), $partner/text()))
  or some $par in $item//par satisfies
   (contains(string($par), "Foobar Corporation")
     and some $partner in $foobar_partners satisfies
        contains(string($par), $partner/text()))
return
    <news_item>
        { $item/title }
        { $item/date }
    </news_item> 

;
(: Q4: Variant of Q2 :)
(: Q4: Find news items where a company and one of its partners is mentioned in the same news item and the news item is not authored by the company itself. :)
for $item in $input//news_item,
    $c in $company-data//company
let $partners := local:partners($c/name)
where contains(string($item), $c/name)
  and some $p in $partners satisfies
    contains(string($item), $p) and $item/news_agent != $c/name
return
    $item 

;
(: Q5: Summarize text :)
(: Q5: For each news item that is relevant to the Gorilla Corporation, create an "item summary" element. :)
for $item in $input//news_item
where contains(string($item/content), "Gorilla Corporation")
return
    <item_summary>
        { $item/title/text() }.
        { $item/date/text() }.
        { string(($item//par)[1]) }
    </item_summary> 

;

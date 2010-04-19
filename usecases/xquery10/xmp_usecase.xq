(: Q1:  Selection and extraction :)
(: Q1:  List books published by Addison-Wesley after 1991, including their year and title. :)
<bib>
  {
   for $b in $bib/bib/book
   where $b/publisher = "Addison-Wesley" and $b/@year > 1991
   return
     <book year="{ $b/@year }">
      { $b/title }
     </book>
  }
</bib>

;
(: Q2:  Flattening :)
(: Q2:  Create a flat list of all the title-author pairs, with each pair enclosed in a "result" element. :)
<results>
   {
     for $b in $bib/bib/book,
         $t in $b/title,
         $a in $b/author
     return
         <result>
             { $t }
             { $a }
         </result>
   }
</results>

;
(: Q3:  Preserving structure :)
(: Q3:  For each book in the bibliography, list the title and authors, grouped inside a "result" element. :)
<results>
{
     for $b in $bib/bib/book
     return
         <result>
             { $b/title }
             { $b/author  }
         </result>
}
</results>

;
(: Q4: Changing structure by nesting :)
(: Q4:  For each author in the bibliography, list the author's name and the titles of all books by that author, grouped inside a "result" element. :)

<results>
  {
    let $a := $bib//author
    for $last in distinct-values($a/last),
        $first in distinct-values($a[last=$last]/first)
    order by $last, $first
    return
        <result>
            <author>
              <last>{ $last }</last>
              <first>{ $first }</first>
            </author>
            {
                for $b in $bib/bib/book
                where some $ba in $b/author 
                      satisfies ($ba/last = $last and $ba/first=$first)
                return $b/title
            }
        </result>
  }
</results>




;
(: Q5:  Combining data sources :)
(: Q5:  For each book found at both bn.com and amazon.com, list the title of the book and its price from each source. :)
<books-with-prices>
   {
     for $b in $bib/bib//book,
         $a in $reviews//entry
     where $b/title = $a/title
     return
         <book-with-prices>
             { $b/title }
             <price-bstore2>{ $a/price/text() }</price-bstore2>
             <price-bstore1>{ $b/price/text() }</price-bstore1>
         </book-with-prices>
   }
</books-with-prices>

;
(: Q6: Counting values :)
(: Q6: For each book that has at least one author, list the title and first two authors, and an empty "et-al" element if the book has additional authors. :)
<bib>
   {
     for $b in $bib/bib//book
     where count($b/author) > 0
     return
         <book>
             { $b/title }
             {
                 for $a in $b/author[position()<=2]
                 return $a
             }
             {
                 if (count($b/author) > 2)
                 then <et-al/>
                 else ()
             }
         </book>
   }
</bib>

;
(: Q7:  Sorting :)
(: Q7:  List the titles and years of all books published by Addison-Wesley after 1991, in alphabetic order. :)
<bib>
   {
     for $b in $bib/bib//book
     where $b/publisher = "Addison-Wesley" and $b/@year > 1991
     order by $b/title 
     return
         <book>
             { $b/@year }
             { $b/title }
         </book>
   }
</bib>

;
(: Q8 : Full-text search operators :)
(: Q8 : Find books in which some element has a tag ending in "or" and the same element contains the string "Suciu" (at any level of nesting). For each such book, return the title and the qualifying element. :)

for $b in $bib/bib//book
let $e := $b/*[contains(string(.), "Suciu")
                and ends-with(local-name(.), "or")]
where exists($e)
return
     <book>
         { $b/title }
         { $e }
     </book>

;
(: Q9 : Full-text search operators :)
(: Q9 : Find all section or chapter titles that contain the word "XML", regardless of the level of nesting :)
<results>
   {
     for $t in $books//(chapter | section)/title
     where contains($t/text(), "XML")
     return $t
   }
</results>

;
(: Q10:  Aggregation :)
(: Q10:  In the document "prices.xml", find the minimum price for each book, in the form of a "minprice" element with the book title asits title attribute. :)
<results>
   {
     let $doc := $prices
     for $t in distinct-values($doc//book/title)
     let $p := $doc//book[title = $t]/price
     return
       <minprice title="{ $t }">
         <price>{ min($p) }</price>
       </minprice>
   }
</results>

;
(: Q11:  context output :)
(: Q11: For each book with an author, return the book with its title and authors. For each book with an editor, return a reference with the book title and the editor's affiliation. :) 


<bib>
{
         for $b in $bib/bib//book[author]
         return
             <book>
                 { $b/title }
                 { $b/author }
             </book>
}
{
         for $b in $bib/bib//book[editor]
         return
           <reference>
             { $b/title }
             {$b/editor/affiliation}
           </reference>
}
</bib>

;
(: Q12: Pairs of books. :)
(: Q12: Find pairs of books that have different titles but the same set of authors (possibly in a different order). :)
<bib>
{
     for $book1 in $bib/bib//book,
         $book2 in $bib/bib//book
     let $aut1 := for $a in $book1/author 
                  order by $a/last, $a/first
                  return $a
     let $aut2 := for $a in $book2/author 
                  order by $a/last, $a/first
                  return $a
     where $book1 << $book2
     and not($book1/title = $book2/title)
     and deep-equal($aut1, $aut2)
     return
         <book-pair>
             { $book1/title }
             { $book2/title }
         </book-pair>
}
</bib>

;

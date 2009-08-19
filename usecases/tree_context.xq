(: ---------------------------------------
    Use Case "Tree" : Operations on trees
   --------------------------------------- :)

declare variable $book1 := doc("docs/book1.xml");

declare function local:toc($book-or-section as element()) as element()*
{
   for $section in $book-or-section/section
   return 
       <section>
          { $section/@*, $section/title, local:toc($section) }
       </section>
};

declare function local:section-summary($book-or-section as element())
  as element()*
{
  for $section in $book-or-section
  return
    <section>
       { $section/@* }
       { $section/title }       
       <figcount>         
         { count($section/figure) }
       </figcount>                
       { for $section1 in $section/section return local:section-summary($section1) }
    </section>
};

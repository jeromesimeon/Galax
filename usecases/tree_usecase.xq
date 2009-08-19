(: Q1: listing all the sections and their titles :)
(: Q1: Prepare a (flat) list for Book1, listing all the sections and their titles. Preserve the original attributes of each <figure> element, if any. :)

<toc>
   {
     for $s in $book1/book return local:toc($s)
   }
</toc>

;
(: Q2: Figure list :)
(: Q2: Prepare a (flat) figure list for Book1, listing all the figures and their titles. Preserve the original attributes of each <figure> element, if any. :)
<figlist>
   {
     for $f in $book1//figure
     return
         <figure>
             { $f/@* }
             { $f/title }
         </figure>
   }
</figlist>

;
(: Q3: Counting :)
(: Q3: How many sections are in Book1, and how many figures? :) 
<section_count>{ count($book1//section) }</section_count>,
<figure_count>{ count($book1//figure) }</figure_count>
;
(: Q4: More counting :)
(: Q4: How many top-level sections are in Book1?  :)
<top_section_count>
  {
    count($book1/book/section)
  }
</top_section_count>

;
(: Q5: Flattening and counting :)
(: Q5: Make a flat list of the section elements in Book1. In place of its original attributes, each section element should have two attributes, containing the title of the section and the number of figures immediately contained in the section. :)
<section_list>
   {
     for $s in $book1//section
     let $f := $s/figure
     return
         <section title="{ $s/title/text() }" figcount="{ count($f) }"/>
   }
</section_list>

;
(: Q6: Nesting :)
(: Q6: Make a nested list of the section elements in Book1, preserving their original attributes and hierarchy. Inside each section element, include the title of the section and an element that includes the number of figures immediately contained in the section. :)
<toc>
   {
     for $s in $book1/book/section
     return local:section-summary($s)
   }
</toc>


;

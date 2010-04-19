(: Q1: "para" descendants :)
(: Q1: Locate all paragraphs in the report (all "para" elements occurring anywhere within the "report" element). :)
<result>
   {
     $report//report//para
   }
</result>



;
(: Q2: Locate all paragraph elements in an introduction :) 
(: Q2: all "para" elements directly contained within an "intro" element :) 
<result>
   {
     $report//intro/para
   }
</result>


;
(: Q3: "para" descendants with no "intro" element :)
(: Q3: All "para" elements directly contained within an "intro" element. :)
<result>
   {
     for $c in $report//chapter
     where empty($c/intro)
     return $c/section/intro/para
   }
</result>

;
(: Q4: Locate specfic section :)
(: Q4: Locate the second paragraph in the third section in the second chapter :)
<result>
   {
     ((($report//chapter)[2]//section)[3]//para)[2]
   }
</result>

;
(: Q5: Locate all classified paragraphs :)
(: Q5: all "para" elements whose "security" attribute has the value "c" :) 
<result>
   {
     $report//para[@security = "c"]
   }
</result>


;
(: Q6: List the short titles of all sections :) 
(: Q6: values of the "shorttitle" attributes of all "section" elements, expressing each short title as the value of a new element :)
<result>
   {
     for $s in $report//section/@shorttitle
     return <stitle>{ $s }</stitle>
   }
</result>


;
(: Q7: Locate the initial letter of the initial paragraph of all introductions :)
(: Q7: the first character in the content [character content as well as element content] of the first "para" element contained in an "intro" element :)
<result>
   {
     for $i in $report//intro/para[1]
     return
         <first_letter>{ substring(string($i), 1, 1) }</first_letter>
   }
</result>

;
(: Q8a: Locate all sections with a title that has "is SGML" in it :)
(: Q8a: all "section" elements that contain a "title" element that has the consecutive characters "is SGML" in its content :)
<result>
   {
     $report//section[.//title[contains(., "is SGML")]]
   }
</result>

;
(: Q8b: Locate all sections with a title that has "is SGML" in it :)
(: Q8b: all "section" elements that contain a "title" element that has the consecutive characters "is SGML" in its content - do not interrupt the sub-element :)
<result>
   {
     $report//section[.//title/text()[contains(., "is SGML")]]
   }
</result>

;
(: Q9: Locate the topics referenced by a cross-reference :)
(: Q9: All "topic" elements whose "topicid" attribute value is the same as an "xrefid" attribute value of any "xref" element :)
<result>
   {
     for $id in $report//xref/@xrefid
     return $report//topic[@topicid = $id]
   }
</result>

;
(: Q10: Locate the cross referecne element which has specific attribute :) 
(: Q10: Locate the closest title preceding the cross-reference ("xref") element whose "xrefid" attribute is "top4" :) 
<result>
   {
     let $x := $report//xref[@xrefid = "top4"],
         $t := $report//title[. << $x]
     return $t[last()]
   }
</result>

;

(: Q1: Recursive parts explosion :)
(: Q1: Convert the sample document from "partlist" format to "parttree" format :)
<parttree>
   {
     for $p in $partlist//part[empty(@partof)]
     return local:one_level($p)
   }
</parttree>


;

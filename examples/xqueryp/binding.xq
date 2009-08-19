let $books := doc("docs/books.xml")/books/book[color = "Red"]
for $i in (1 to count($books))
return
 { replace value of node $books[$i]/color with "Blue";
   avg($books/price)}


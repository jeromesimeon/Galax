#!/bin/sh
for i in n1 n2 n3 n4 n5
do
  echo 'module namespace N = "narada";' > run/$i.xq
  echo 'import interface namespace S = "narada" at "narada.xqi";' >> run/$i.xq
  echo 'import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";' >> run/$i.xq
#  echo 'declare default element namespace "narada";' >> run/$i.xq
  echo 'declare variable $local:self := "'$i'";' >> run/$i.xq 
  cat narada.xq >> run/$i.xq
  cp control.xml run
done

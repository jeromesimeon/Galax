#!/bin/sh
for i in 1*.xml
do
  j=`basename $i .xml`
  echo 'module namespace D = "dns";' > run/$j.xq
  echo 'import interface namespace S = "dns" at "file:dns.xqi";' >> run/$j.xq
  echo 'import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";' >> run/$j.xq
  echo 'declare namespace N = "graph";' >> run/$j.xq
  echo 'declare variable $local:self := "'$j'";' >> run/$j.xq
  echo 'declare variable $local:rr := '>> run/$j.xq
  cat $i >> run/$j.xq
  echo ';' >> run/$j.xq
  cat dns.xq >> run/$j.xq
done
cp control.xml run

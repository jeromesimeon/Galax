#!/bin/sh
# 221 is chronically ill
# We can use 18-29 ONLY
for i in 181 191 201 211  231 241 251 261 271 281 291 
do
  echo 'module namespace N = "narada";' > cluster/cactus$i.xq
  echo 'import interface namespace S = "narada" at "narada.xqi";' >> cluster/cactus$i.xq
  echo 'import module namespace dxq = "http://www.galaxquery.org/dxq" at "../../dxq.xq";' >> cluster/cactus$i.xq
  echo 'declare variable $self := "cactus'$i'";' >> cluster/cactus$i.xq
  cat narada.xq >> cluster/cactus$i.xq
  cp control.xml cluster
done

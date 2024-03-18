#!/bin/sh
commentoutfile=exclude.txt
origfile=Finland_clean_allCC.txt.good
outfile=Finland_clean_allCC.txt
badfile=bad.txt
if test ! -r ${commentoutfile}
then
  echo "No file ${commentoutfile} found"
  exit 0
fi
if test ! -r ${origfile}
then
  echo "No file ${origfile} found"
  exit 0
fi
sort ${origfile} > sorted.txt
if test -r ${badfile}
then
  rm ${badfile}
fi
touch ${badfile}
while read line
do
  set $line
  stat=$1
  phas=$2
  ev=$3
  awk '($1 == EV || $2 == EV ) && $5 == STAT && $6 == PHASE' EV=$ev STAT=$stat PHASE=$phas sorted.txt > ${badfile}
done < ${commentoutfile}
sort ${badfile} > bad_sorted.txt
diff sorted.txt bad_sorted.txt | grep -v "\," | sed 's_< __g' > diff.txt
mv diff.txt ${outfile}

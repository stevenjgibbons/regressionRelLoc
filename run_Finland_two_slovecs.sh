#!/bin/sh
program=oneSlovecPairVectorSolve
exe=./bin/${program}
if test ! -r ${exe}
then
  echo "No program ${exe} found ... "
  exit 1
fi
scriptname=./run_Finland_two_slovecs.sh
if [ $# != 4 ]
then
  echo
  echo "USAGE: "
  echo "$scriptname   stat1  phase1  stat2 phase2  "
  echo "$scriptname   KEV    P1      SGF   S1      "
  echo "$scriptname   LP61   P1      LP61  S1      "
  echo "$scriptname   LP53   P1      LP61  P1      "
  echo "$scriptname   LP53   S1      LP61  S1      "
  echo "$scriptname   ARE0   P1      ARE0  S1      "
  echo
  exit 1
fi
#
stat1=$1
phas1=$2
stat2=$3
phas2=$4
evdir=FinlandFiles
slovecsfile=${evdir}/Finland_ak135_slovecs.txt
locationsfile=${evdir}/Finland_events_xy.txt
reltimesfile=${evdir}/Finland_clean_allCC.txt
for file in \
  ${slovecsfile} \
  ${locationsfile} \
  ${reltimesfile}
do
  if test ! -r ${file}
  then
    echo "No file ${file} found "
    exit 1
  fi
done
nsp=`wc ${slovecsfile} | awk '{print $1}'`
nev=`wc ${locationsfile} | awk '{print $1}'`
infile=${program}.input
if test -r ${infile}
then
  mv ${infile} ${infile}.old
fi
cp ${slovecsfile} ${infile}
cat ${locationsfile} >> ${infile}
cat ${reltimesfile}  >> ${infile}
${exe} ${nsp} ${nev} ${stat1} ${phas1} \
        ${stat2} ${phas2}   < ${infile}

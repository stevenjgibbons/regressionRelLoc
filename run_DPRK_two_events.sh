#!/bin/sh
program=oneEventPairVectorSolve
exe=./bin/${program}
if test ! -r ${exe}
then
  echo "No program ${exe} found ... "
  exit 1
fi
scriptname=./run_DPRK_two_events.sh
if [ $# != 2 ]
then
  echo
  echo "USAGE: "
  echo "$scriptname   event1    event2 "
  echo "$scriptname   DPRK2     DPRK1  "
  echo
  exit 1
fi
#
event1=$1
event2=$2
evdir=DPRKfiles
slovecsfile=${evdir}/DPRK_ak135_slovecs.txt
locationsfile=${evdir}/DPRK_events_zero.txt
reltimesfile=${evdir}/DPRK_clean_allCC.txt
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
infile=oneEventPairVectorSolve.input
if test -r ${infile}
then
  mv ${infile} ${infile}.old
fi
cp ${slovecsfile} ${infile}
cat ${locationsfile} >> ${infile}
cat ${reltimesfile}  >> ${infile}
${exe} ${nsp} ${nev} ${event1} ${event2} < ${infile}

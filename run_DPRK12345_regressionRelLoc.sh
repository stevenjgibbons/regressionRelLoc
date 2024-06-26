#!/bin/sh
# Removes DPRK6
dtol=0.0055
mxiter=10000
DSLOWM=0.025
program=regressionRelLoc
exe=./bin/${program}
if test ! -r ${exe}
then
  echo "No program ${exe} found ... "
  exit 1
fi
scriptname=./run_DPRK12345_regressionRelLoc.sh
if [ $# != 1 ]
then
  echo
  echo "USAGE: "
  echo "$scriptname   refevent1   "
  echo "$scriptname   DPRK2       "
  echo
  exit 1
fi
#
refevent=$1
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
# nev=`wc ${locationsfile} | awk '{print $1}'`
nev=5
infile=${program}.input
if test -r ${infile}
then
  mv ${infile} ${infile}.old
fi
cp ${slovecsfile} ${infile}
grep -v DPRK6 ${locationsfile} >> ${infile}
grep -v DPRK6 ${reltimesfile}  >> ${infile}
${exe} ${nsp} ${nev} ${refevent} ${dtol} ${mxiter} ${DSLOWM} < ${infile} | grep -v resid_diff | grep -v dxj-dxi | grep -v dxb-dxa | grep -v Observation

#!/bin/sh
dtol=0.004 
mxiter=10000
dslowm=0.015
program=regressionRelLoc
exe=./bin/${program}
if test ! -r ${exe}
then
  echo "No program ${exe} found ... "
  exit 1
fi
scriptname=./run_DPRK_regressionRelLoc.sh
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
# slovecsfile=${evdir}/DPRK12345slow.txt
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
infile=${program}.input
if test -r ${infile}
then
  mv ${infile} ${infile}.old
fi
cp ${slovecsfile} ${infile}
cat ${locationsfile} >> ${infile}
# grep -v DPRK6 ${reltimesfile}  >> ${infile}
cat ${reltimesfile}  >> ${infile}
# ${exe} ${nsp} ${nev} ${refevent} ${dtol} ${mxiter} ${dslowm} < ${infile} 
${exe} ${nsp} ${nev} ${refevent} ${dtol} ${mxiter} ${dslowm} < ${infile} | grep -v resid_diff | grep -v Obse 

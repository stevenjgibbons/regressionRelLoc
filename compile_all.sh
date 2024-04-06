#!/bin/sh
cd SUBS
make
cd ..
if test ! -r subslib.a
then
  echo "Failed to create library subslib.a"
fi
make oneEventPairVectorSolve
make oneSlovecPairVectorSolve
make fixedSlovecsEventSolve
make fixedEventsSlovecsSolve
make regressionRelLoc
make checkcircvc
for file in \
   bin/oneEventPairVectorSolve \
   bin/oneSlovecPairVectorSolve \
   bin/fixedSlovecsEventSolve \
   bin/fixedEventsSlovecsSolve \
   bin/regressionRelLoc \
   bin/checkcircvc
do
  if test ! -r $file
  then
    echo Failed to create executable $file
  fi
  chmod 755 $file
done
chmod 755 run_Finland_two_slovecs.sh
chmod 755 run_DPRK_two_events.sh
chmod 755 run_Finland_two_events.sh
chmod 755 run_DPRK_abs_events.sh
chmod 755 run_Finland_abs_events.sh
chmod 755 run_DPRK_regressionRelLoc.sh
chmod 755 run_Finland_regressionRelLoc.sh
chmod 755 run_DPRK12345_regressionRelLoc.sh
chmod 755 run_DPRK_abs_eventsAK135.sh
chmod 755 run_DPRK_abs_eventsCORRECTED.sh
chmod 755 run_Finland_abs_eventsAK135.sh
chmod 755 run_Finland_abs_eventsREGRELLOC.sh
chmod 755 XY2LATLON/run_xy2latlon.sh
chmod 755 XY2LATLON/xy2latlon.py

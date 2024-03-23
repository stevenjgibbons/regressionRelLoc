#!/bin/sh
#attempt1 reflat=41.29520
#attempt1 reflon=129.07780
#attempt2 reflat=41.29520
#attempt2 reflon=129.08500
reflat=41.29520
reflon=129.08400
#
echo AK135_DPRK_locations_xyC.txt
python xy2latlon.py --reflat $reflat --reflon $reflon --infile AK135_DPRK_locations_xyC.txt
echo
#
echo REGRELLOC_DPRK_locations_xyC.txt
python xy2latlon.py --reflat $reflat --reflon $reflon --infile REGRELLOC_DPRK_locations_xyC.txt
echo
#

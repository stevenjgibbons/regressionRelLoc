# regressionRelLoc
A suite of programs for calculating the relative locations of seismic events and the corresponding slowness vectors

[![SQAaaS badge](https://github.com/EOSC-synergy/SQAaaS/raw/master/badges/badges_150x116/badge_software_bronze.png)](https://api.eu.badgr.io/public/assertions/ncpA6jz6S9ajG8uvSar2rg "SQAaaS bronze badge achieved")  

[![SQAaaS badge shields.io](https://img.shields.io/badge/sqaaas%20software-bronze-e6ae77)](https://api.eu.badgr.io/public/assertions/ncpA6jz6S9ajG8uvSar2rg "SQAaaS bronze badge achieved")  

Release v1.0.3 is permanently stored on Zenodo with DOI https://doi.org/10.5281/zenodo.11046217  

https://zenodo.org/doi/10.5281/zenodo.11046216  

[![DOI](https://zenodo.org/badge/773051705.svg)](https://zenodo.org/doi/10.5281/zenodo.11046216)  

The main program for iterative solution of both locations and slowness vectors, from an initial specification of slowness vectors, is called regressionRelLoc.  
The program fixedSlovecsEventSolve solves for a set of events for a given set of fixed slowness vectors.  
The program fixedEventsSlovecsSolve solves for the optimal slowness vectors for a given set of event locations.  
(regressionRelLoc basically iterates between the operations performed in fixedSlovecsEventSolve and the operations performed in fixedEventsSlovecsSolve.)  
There are two additional programs, oneEventPairVectorSolve and oneSlovecPairVectorSolve, which were basically written for unit testing of the operations in regressionRelLoc.  
Before we call the codes, we need to define a set of slowness vectors. The program statphase2slowvec (https://github.com/stevenjgibbons/statphase2slowvec) will calculate the slowness vectors for a set of stations and phases using the ak135 model.  
The Cartesian output can be transformed back to geographical space using the [xy2latlon.py](https://github.com/stevenjgibbons/regressionRelLoc/blob/main/XY2LATLON/xy2latlon.py) program in this repository.  

There are two example cases:  

(1) The relative location benchmark study of Gibbons et al. (2020), consisting of 55 surface explosions in northern Finland.  

(2) The 6 declared underground nuclear tests conducted by the DPRK between October 2006 and September 2017.  

The programs are as follows:  

```
oneEventPairVectorSolve
```
Here we assume a fixed set of slowness vectors and specify two event codes, eventA and eventB.  
It will return (rx_eventB - rx_eventA, ry_eventB - ry_eventA) given in km.  
(See the scripts *run_Finland_two_events.sh* and *run_DPRK_two_events.sh*)  

```
oneSlovecPairVectorSolve
```
Here we assume a fixed set of event coordinates (specified as x and y in km) and specify two slowness vectors, i.e. station_i, phase_i, station_j, phase_j.  
It will return (sx_j - sx_i, sy_j - sy_i ) in seconds per km.  
(see the script *run_Finland_two_slovecs.sh*).  

```
fixedSlovecsEventSolve
```
Here we assume a fixed set of slowness vectors and specify a reference event. It then solves for the locations of the other events relative to this event.
(See the scripts *run_Finland_abs_events.sh* and *run_DPRK_abs_events.sh*)  

```
fixedEventsSlovecsSolve
```
Here we assume a fixed set of events and solve for the slowness vectors that best fit these.
(See the scripts *run_Finland_abs_slovecs.sh* and *run_DPRK_abs_slovecs.sh*)  

```
regressionRelLoc
```
Here we iterate to solve both slowness vectors and event locations.
(See the script *run_Finland_regressionRelLoc.sh*)




**Compiling instructions**  

(1) Edit Makefile to sensure that the LAPACK and BLAS libraries exist. e.g.  

```
LAPACK= /usr/lib/x86_64-linux-gnu/liblapack.a
BLAS= /usr/lib/x86_64-linux-gnu/libblas.a
```

(2) Run the script *compile_all.sh*  

```
sh compile_all.sh
```

The software has been tested using gfortran Ubuntu 11.4.0-1ubuntu1~22.04.

All programs use only Cartesian coordinates (rx,ry) for the event locations and (sx,sy) for the slowness vectors.  

We need to generate the slowness vectors, for example using the program *statphase2slowvec* (https://github.com/stevenjgibbons/statphase2slowvec) and we provide in this repository a little auxiliary program *xy2latlon* to transform from the x, y event coordinates to latitude and longitude.  



**References**  

Gibbons, S.J., Kvaerna, T., Tiira, T., Kozlovskaya, E., 2020.  
A benchmark case study for seismic event relative location,  
Geophys J Int, 223, 1313-1326  


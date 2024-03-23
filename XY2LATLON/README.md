
This directory just contains a little tool from converting text files with  
```
EVENTID    dx_km    dy_km      resid
```

to files with latitutde and longitude, if we specify a reference latitude and a reference longitude to correspond to X=0, Y=0.  

For example, we arrive at the following two files from the *regressionRelLoc* program:  

(1) *AK135_DPRK_locations_xyC.txt*  

```
DPRK1     2.3935    -0.6661     0.0360
DPRK2     0.0000     0.0000     0.0188
DPRK3    -0.3402    -0.3007     0.0185
DPRK4    -0.6830     0.3365     0.0183
DPRK5    -0.1448     0.3820     0.0174
DPRK6    -0.3393     0.4513     0.0258
```

and  

(2) *REGRELLOC_DPRK_locations_xyC.txt*  

```
DPRK1     1.7249    -0.4365     0.0141
DPRK2     0.0000     0.0000     0.0102
DPRK3    -0.2679    -0.2940     0.0070
DPRK4    -0.5288     0.2667     0.0077
DPRK5    -0.1238     0.3732     0.0077
DPRK6    -0.3643     0.5184     0.0178
```  

We want to be able to specify a reference latitude and a reference longitude and find their geographical coordinates and
information about relative distances and directions.  

Running the script *run_xy2latlon.sh* generates the output  

```
AK135_DPRK_locations_xyC.txt
DPRK1   41.2892   129.1126   2.3935  -0.6661   0.0360
DPRK2   41.2952   129.0840   0.0000   0.0000   0.0188
DPRK3   41.2925   129.0799  -0.3402  -0.3007   0.0185
DPRK4   41.2982   129.0758  -0.6830   0.3365   0.0183
DPRK5   41.2986   129.0823  -0.1448   0.3820   0.0174
DPRK6   41.2993   129.0799  -0.3393   0.4513   0.0258

REGRELLOC_DPRK_locations_xyC.txt
DPRK1   41.2913   129.1046   1.7249  -0.4365   0.0141
DPRK2   41.2952   129.0840   0.0000   0.0000   0.0102
DPRK3   41.2926   129.0808  -0.2679  -0.2940   0.0070
DPRK4   41.2976   129.0777  -0.5288   0.2667   0.0077
DPRK5   41.2986   129.0825  -0.1238   0.3732   0.0077
DPRK6   41.2999   129.0797  -0.3643   0.5184   0.0178
``` 

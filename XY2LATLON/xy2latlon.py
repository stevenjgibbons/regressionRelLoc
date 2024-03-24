#!/usr/bin/env python3
#
# Steven J. Gibbons
# 2024-03-23
#
# We have an input file with the format
# EVENTID   xkm  ym   value
# and we take filename, reflat, and reflon
# as input arguments and calculate the latitude, longitude of
# all points.
#
# xy2latlon.py
#
try:
    import os
    import sys
    import argparse
    import statistics
    from scipy import stats
    import random
    import numpy as np
    import geographiclib
    from geographiclib.geodesic import Geodesic
    import math
except ImportError as ie:
    miss_mod = ie.args[0].split()[3]
    print("\nThe Python module '" + miss_mod + "' is required.")
    print("Please install it and run again.\n")
    exit(1)

#==========================================================================
def file_exist(file):
    """Check if a file (with full path) exist"""
    if not os.path.isfile(file):
        print("File: ",file," does not exist. Bailing out ...")
        exit()

#==========================================================================
class llLocation:
    def __init__( self, lat, lon ):
        self.lat = lat
        self.lon = lon
        self.x   = 0.0
        self.y   = 0.0

    def incxy( self, incxkm, incykm ):
        self.x = self.x + incxkm
        self.y = self.y + incykm
        azim_rad = math.atan2( self.x, self.y )
        azim_deg = math.degrees( azim_rad )
        geod = Geodesic.WGS84
        reflat = self.lat
        reflon = self.lon
        distkm = math.sqrt( self.x * self.x + self.y * self.y )
        g = geod.Direct( reflat, reflon, azim_deg, 1000.0 * distkm )
        self.lat = g['lat2']
        self.lon = g['lon2']

    def xyLoc2llLoc( self ):
        azim_rad = math.atan2( self.x, self.y )
        azim_deg = math.degrees( azim_rad )
        geod = Geodesic.WGS84
        reflat = self.refloc.lat
        reflon = self.refloc.lon
        distkm = math.sqrt( self.x * self.x + self.y * self.y )
        g = geod.Direct( reflat, reflon, azim_deg, 1000.0 * distkm )
        return llLocation( g['lat2'], g['lon2'] )

#==========================================================================
def dist_between_locs_km( loc1, loc2 ):
    geod = Geodesic.WGS84
    g = geod.Inverse( loc1.lat, loc1.lon, loc2.lat, loc2.lon )
    return 0.001 * g['s12']

#==========================================================================
def dist_between_locs_deg( loc1, loc2 ):
    geod = Geodesic.WGS84
    g = geod.Inverse( loc1.lat, loc1.lon, loc2.lat, loc2.lon )
    return g['a12']

#==========================================================================
def source_to_receiver_azimuth( rloc, sloc ):
    geod = Geodesic.WGS84
    g = geod.Inverse( sloc.lat, sloc.lon, rloc.lat, rloc.lon )
    azimuth = g['azi1']
    if ( azimuth < 0.0 ):
        azimuth = azimuth + 360.0
    return azimuth

#==========================================================================
def receiver_to_source_backazimuth( rloc, sloc ):
    geod = Geodesic.WGS84
    g = geod.Inverse( rloc.lat, rloc.lon, sloc.lat, sloc.lon )
    backazimuth = g['azi1']
    if ( backazimuth < 0.0 ):
        backazimuth = backazimuth + 360.0
    return backazimuth

#==========================================================================
def new_location_azi_distkm( loc1, azi, distkm ):
    geod = Geodesic.WGS84
    g = geod.Direct( loc1.lat, loc1.lon, azi, 1000.0 * distkm )
    return llLocation( g['lat2'], g['lon2'] )

#==========================================================================
def read_xyfile( filename, reflat, reflon ):
    file_exist( filename )
    infile = open( filename, 'r' )
    Locs   = []
    for line in infile:
        words  = line.split()
        evid   = words[0]
        xkm    = float( words[1] )
        ykm    = float( words[2] )
        value  = float( words[3] )
        evloc = llLocation( reflat, reflon )
        evloc.incxy( xkm, ykm )
        lat = evloc.lat
        lon = evloc.lon
        latstring = "{:.6f}".format(     lat ).rjust(11)
        lonstring = "{:.6f}".format(     lon ).rjust(12)
        xkmstring = "{:.4f}".format(     xkm ).rjust( 8)
        ykmstring = "{:.4f}".format(     ykm ).rjust( 8)
        valstring = "{:.4f}".format(   value ).rjust( 8)
        print( evid, latstring, lonstring, xkmstring, ykmstring, valstring )

#
scriptname = sys.argv[0]
numarg     = len(sys.argv) - 1
text       = 'Specify '
text      += '--reflat [reflat] '
text      += '--reflon [reflon] '
text      += '--infile [infile] '

parser     = argparse.ArgumentParser( description = text )
parser.add_argument("--reflat", help="Reference latitude", default=None, required=True )
parser.add_argument("--reflon", help="Reference longitude", default=None, required=True )
parser.add_argument("--infile", help="input file", default=None, required=True )

args = parser.parse_args()

reflat           = float( args.reflat  )
reflon           = float( args.reflon  )
infile           = args.infile

read_xyfile( infile, reflat, reflon )

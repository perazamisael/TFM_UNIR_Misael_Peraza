#! /bin/csh
#
# PURPOSE:
# Script to load environment variables
# required by TRACKFIL4HFC and
# build bin file
# Create required directories if not found
# Must be placed in the hfc/scripts sub-directory.
#
# USAGE:
#   source setup_trackfil4hfc.csh
#
# RESTRICTIONS/COMMENTS:
#  be sure to have SolarSoft (SSW) ready to be
#  called with "sswidl" command
#
# X.Bonnin, 20-NOV-2015

# Get script dir
set currentdir=`pwd`
set sourced=($_)

if ("$sourced" != "") then
    set scriptpath=`dirname $sourced[2]`
endif
if ("$0" != "tcsh") then
    set scriptpath=`dirname $0`
endif
cd $scriptpath

# Load Trackfil4hfc env. variables
csh -f setup_trackfil4hfc_env.csh

sswidl -e @trackfil4hfc_make_bin

cd $currentdir

#! /bin/csh
#
# PURPOSE:
# Script to load environment variables
# required by TRACKFIL and
# build bin file
# Create required directories if not found
# Must be placed in the scripts/ sub-directory.
#
# USAGE:
#   source setup_trackfil.csh
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
    echo "SCRIPTPATH1" $scriptpath
endif
if ("$0" != "tcsh") then
    set scriptpath=`dirname $0`
    echo "$0 -- $1 --  $2 -- $_ -- SCRIPTPATH2" $scriptpath
endif
cd $scriptpath
echo "SCRIPTPATH" $scriptpath

# Load Trackfil4hfc env. variables
csh -f setup_trackfil_env.csh

sswidl -e @trackfil_make_bin

cd $currentdir

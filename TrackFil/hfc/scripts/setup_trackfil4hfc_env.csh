#! /bin/csh
#
# Script to load environment variables
# required by TRACKFIL4HFC.
# Create required directories if not found
# Must be placed in the hfc/scripts sub-directory.
#
# Usage:
#   source setup_trackfil4hfc.csh
#
# X.Bonnin, 20-NOV-2015

# Define trackfil home directory

set currentdir=`pwd`
set sourced=($_)
if ("$sourced" != "") then
    set scriptpath=`dirname $sourced[2]`
endif
if ("$0" != "tcsh") then
    set scriptpath=`dirname $0`
endif
cd $scriptpath/..
setenv TRACKFIL4HFC_HOME_DIR `pwd`
cd $TRACKFIL4HFC_HOME_DIR/..
setenv TRACKFIL_HOME_DIR `pwd`
cd $currentdir

# Append idl library path to $IDL_PATH
setenv IDL_PATH "$IDL_PATH":+$TRACKFIL4HFC_HOME_DIR/wrapper

# Load Trackfil env. variables
source $TRACKFIL_HOME_DIR/scripts/setup_trackfil_env.csh

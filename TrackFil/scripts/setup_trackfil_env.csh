#! /bin/csh
#
# Script to load environment variables
# required by TRACKFIL.
# Create required directories if not found
# Must be placed in the /scripts sub-directory.
#
# Usage:
#   source setup_trackfil.csh
#
# X.Bonnin, 20-NOV-2015

# Define trackfil home directory

if ( -z "$TRACKFIL_HOME_DIR ") then
    set currentdir=`pwd`
    set sourced=($_)
    if ("$sourced" != "") then
        set scriptpath=`dirname $sourced[2]`
    endif
    if ("$0" != "tcsh") then
        set scriptpath=`dirname $0`
    endif
    cd $scriptpath/..
    echo "about to set trackfil_home_dir" `pwd`
    setenv TRACKFIL_HOME_DIR `pwd`
    cd $currentdir
endif

# Append idl library path to $IDL_PATH
setenv IDL_PATH "$IDL_PATH":+$TRACKFIL_HOME_DIR/src

# Append idl library path to $IDL_PATH
setenv IDL_PATH "$IDL_PATH":+$TRACKFIL_HOME_DIR/lib/idl

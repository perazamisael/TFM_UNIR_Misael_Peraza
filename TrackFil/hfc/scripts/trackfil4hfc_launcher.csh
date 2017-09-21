#! /bin/sh -f

# Script file to run trackfil code calling a runtime IDL program
# from a python script
# Usage: sh trackfil_hfc_launcher.sh config_file starttime endtime
# X.Bonnin (LESIA), 20-06-2012

# Directory definitions
SRC_DIR=/obs/helio/hfc/frc/trackfil
PRODUCT_DIR=$SRC_DIR/products

IDL_BIN_FILE=$SRC_DIR/lib/idl/bin/trackfil_hfc.sav

#Input argument definitions
set CONFIG_FILE=$1
set STARTTIME=$2
set ENDTIME=$3

echo "Write python script to call trackfil"

#! /bin/bash

#====================================================
# file: link.sh
#----------------------------------------------------
# - link a secure module to an outside test / attack
# By Ajhl
#====================================================

Usage="link.sh module.o partner.c"
Outfold="out/"

# script input
if [[ $# -lt 2 ]];then
    echo $Usage
    exit 1
fi

module=$1
partner=$2

#==== format : format errors ====
function format
{
    echo "------------------------------------------------------"
    echo "$1 failed"
    echo "------------------------------------------------------"
    cat $2
}

#==== toplevel ====

# compile the partner
pfilen=$(basename "$partner" 2> /dev/null) 
pbase="${pfilen%.*}"
pobj=${Outfold}/${pbase}.o
tempp=/tmp/${pbase}.tmp

gcc $partner -I./lib/ -g -Werror -Wfatal-errors -O -c -o $pobj  > $tempp 2>&1
if [[ $? -ne 0 ]];then
    format "Compiling $partner !" $tempp
    exit 3
fi

# link with the module
filename=$(basename "$module" 2> /dev/null) 
base="${filename%.*}"
outputb="${Outfold}/${base}.bin"
temp=/tmp/${base}.tmp

gcc $module $pobj -o $outputb > $temp 2>&1
if [[ $? -ne 0 ]];then
    format "Linking $module $partner" $temp
    exit 3
fi
exit 0

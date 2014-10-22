#! /bin/bash

#====================================================
# file: compile.sh
#----------------------------------------------------
# - Compile miniml into a c module
# By Ajhl
#====================================================

usage="compile.sh input"
compiler="./main.native"
outf="out/"


if [[ $# -lt 1 ]];then
    echo $usage
    exit 2
fi

input=$1
filename=$(basename "$1" 2> /dev/null) 
extension="${filename##*.}"
base="${filename%.*}"
outputc="${outf}${base}.c"
outputm="${outf}${base}.o"

$compiler < $input > $outputc

if [[ $? -ne 0 ]];then
    echo "MiniML compiler failed"
    echo " Result of :: $outputc"
    cat $outputc 
    exit 2
fi

gcc  $outputc -I./lib/ -O -c -o $outputm

if [[ $? -ne 0 ]];then
    echo "------------------------------------------------------"
    echo "C Compilation failed"
    echo "------------------------------------------------------"
    cat $outputc 
fi


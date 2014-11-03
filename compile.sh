#! /bin/bash

#====================================================
# file: compile.sh
#----------------------------------------------------
# - Compile miniml into a c module
# By Ajhl
#====================================================

Usage="compile.sh input"
Compiler="./main.native"
Outfold="out/"


if [[ $# -lt 1 ]];then
    echo $Usage
    exit 2
fi

input=$1
filename=$(basename "$1" 2> /dev/null) 
extension="${filename##*.}"
base="${filename%.*}"
outputc="${Outfold}${base}.c"
outputm="${Outfold}${base}.o"

$Compiler < $input > $outputc

if [[ $? -ne 0 ]];then
    echo "MiniML compiler failed"
    echo " Result of :: $outputc"
    cat $outputc 
    exit 1
fi

gcc  $outputc -I./lib/ -g -Werror -Wfatal-errors -O -c -o $outputm 

if [[ $? -ne 0 ]];then
    echo "------------------------------------------------------"
    echo "C Compilation failed"
    echo "------------------------------------------------------"
    cat $outputc 
    exit 2
fi

exit 0

#!/bin/bash

#====================================================
# file: test.sh
#----------------------------------------------------
# - Test all examples for compilation and tests/
# By Ajhl
#====================================================

Location="test-cases"
Testfold="tests"
Logfold="log"
Compiler=./compile.sh
Linker=./link.sh
Outfold="out"

#==== newlog : spawn new log ====
function newlog
{
    local unique=$(date +"%m-%d-%H-%M-%S")
    local newlog="$Logfold/test_$unique"
    echo > $newlog
    echo "$newlog"
}

#==== printr : print succes or failure  ====
function printr
{
    if [[ $# -lt 0 ]];then
        exit 1;
    else
        if [[ $1 -ne 0 ]];then
            echo -e "\033[1;31m[FAIL]\033[0m"
        else 
            echo -e "\033[1;33m[PASS]\033[0m"
        fi
    fi
}


#==== toplevel ====
logname=$(newlog)
echo "== All output stored in: $logname =="

for file in $Location/*.miniml
do
    tabs 4
    name=${file##*/}
    base=${name%.miniml}
    echo "" >> $logname
    echo "" >> $logname
    echo "#==========================================#" >> $logname
    echo "# Result for $base" >> $logname
    echo "#==========================================#" >> $logname
    echo "" >> $logname
    echo "Testing :: $base"

    # test compilation and abort further tests if failed
    echo -ne "\tCompilation :: "
    $Compiler $file >> $logname 2>&1
    res=$?
    printr $res
    if [[ $res -ne 0 ]];then
        echo -ne "\t--> "
        if [[ $res -ne 2 ]];then
            echo "MiniML Compiler Failed !"
        else
            echo "Compilation of C code Failed !"
        fi
        continue
    fi
    
    # the specific tests for this file
    for testf in $Testfold/*.c
    do
        testname=${testf##*/}
        testbase=${name%.c}
        if [[ $testname =~ ^$base ]];then
            uppname=$(echo $testname | tr '[:lower:]' '[:upper:]')
            echo -e "\t==> \033[1;34m${uppname}\033[0m <== "  
            tabs 8

            # Attempt to link - next if failure
            echo -ne "\tLinking :: "
            $Linker ${Outfold}/${base}.o ${testf} >> $logname 2>&1
            res=$?
            printr $res
            if [[ $res -ne 0 ]];then
                echo -ne "\t--> "
                if [[ $res -ne 2 ]];then
                    echo "Could not compile  $testname!"
                fi
                continue
            fi

            # The actual test
            echo -ne "\tExecution :: "
            ./${Outfold}/${base}.bin >> $logname 2>&1 
            printr $? 
        fi
    done
done

exit 0

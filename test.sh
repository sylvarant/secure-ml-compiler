#!/bin/bash

Location="test-cases"
Logfold="log"

#==== newlog : spawn new log ====
function newlog
{
    local unique=$(date +"%m-%d-%H-%M-%S")
    local newlog="$Logfold/test_$unique"
    echo > $newlog
    echo "$newlog"
}


#==== toplevel ====
logname=$(newlog)
echo "== All output stored in: $logname =="

for file in $Location/*.miniml
do
    name=${file##*/}
    base=${name%.miniml}
    echo "" >> $logname
    echo "" >> $logname
    echo "#==========================================#" >> $logname
    echo "# Result for $base" >> $logname
    echo "#==========================================#" >> $logname
    echo "" >> $logname
    echo -n "Testing :: $base"
    ./compile.sh $file >> $logname 2>&1
    if [[ $? -ne 0 ]];then
        echo -e " \033[1;31m[FAILED]\033[0m"
    else
        echo -e " \033[1;33m[SUCCEEDED]\033[0m"
    fi
done


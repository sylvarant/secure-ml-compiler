#!/bin/bash

Location="test-cases"

for file in $Location/*.miniml
do
    name=${file##*/}
    base=${name%.miniml}
    echo "Testing:: $base"
    ./main.native < $file 
done


#!/bin/bash

# Function to start measuring time
start_measuring_time() {
    START_TIME=$(python3 -c 'import time; print(int(time.time() * 1000))')
}

# Function to stop measuring time
stop_measuring_time() {
    END_TIME=$(python3 -c 'import time; print(int(time.time() * 1000))')
}

# Function to get elapsed time in ms
elapsed_time_in_ms() {
    echo "$((END_TIME - START_TIME))"
}

for f in ./Random3SAT/vars*.cnf
do
    echo
    echo "------------------"
    echo $f
    echo "KisSAT:"
    start_measuring_time
    ./kissat-master/build/kissat -v $f > outKisSAT
    stop_measuring_time
    kisSAT_time=$(elapsed_time_in_ms)
    grep -Eo "UNSATISFIABLE|SATISFIABLE" outKisSAT
    grep "decisions" outKisSAT
    echo "${kisSAT_time} ms"

    echo "misat:"
    start_measuring_time
    ./misat < $f
    stop_measuring_time
    misat_time=$(elapsed_time_in_ms)
    echo "${misat_time} ms"

    if [ "$misat_time" -ne 0 ]; then
        ratio=$(echo "scale=2; $misat_time / $kisSAT_time" | bc)
        echo "Time ratio (KisSAT/misat): ${ratio}"
    else
        echo "kisSAT took 0 ms, cannot calculate ratio."
    fi
done


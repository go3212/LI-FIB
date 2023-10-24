#!/bin/bash

start_measuring_time() {
  # Record start time in milliseconds
  start=$(date +%s%3N)
}

stop_measuring_time() {
  # Record end time in milliseconds
  end=$(date +%s%3N)
}

elapsed_time_in_ms() {
  # Calculate elapsed time in milliseconds
  echo $((end-start))
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
    egrep -o "UNSATISFIABLE|SATISFIABLE" outKisSAT
    egrep "decisions" outKisSAT
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

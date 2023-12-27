#! /bin/bash

# Evals a single erlang expression with the aoc code loaded. aaa

set -e
trap 'echo >&2 "Error - exited with status $? at line $LINENO:";
         pr -tn $0 | tail -n+$((LINENO - 3)) | head -n7' ERR


QUIET=1 rebar3 compile

if  [ "$#" -eq 0 ] ; then
    ./run.sh "aoc_solution:run()."
elif [ "$#" -eq 1 ] ; then 
    ./run.sh "aoc_solution:run(${1})."
elif [ "$#" -eq 2 ] ; then 
    ./run.sh "aoc_solution:run({${1}, ${2}})."  

elif [ "$#" -eq 3 ] ; then 
    if  [ -f "${3}" ]; then
        file=$(realpath  "${3}")
        ./run.sh "aoc_solution:run({${1}, ${2}}, all, \"${file}\")."  
    else
        ./run.sh "aoc_solution:run({${1}, ${2}}, all, ${3})."  
    fi
elif [ "$#" -eq 4 ] ; then 
     if  [ -f "${4}" ]; then
        file=$(realpath  "${4}")
        ./run.sh "aoc_solution:run({${1}, ${2}}, ${3}, \"${file}\")."  
    else
        ./run.sh "aoc_solution:run({${1}, ${2}}, ${3}, ${4})."  
    fi
fi
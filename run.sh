#! /bin/bash

# Evals a single erlang expression with the aoc code loaded. aaa

set -e
trap 'echo >&2 "Error - exited with status $? at line $LINENO:";
         pr -tn $0 | tail -n+$((LINENO - 3)) | head -n7' ERR

[ "$#" -eq 1 ]
QUIET=1 rebar3 compile

command="${1}"
last="${1:(-1)}"

if [ "$last" = "." ]; then
    erl -pa $(rebar3 path) -noshell -eval "application:ensure_all_started(aoc)"  -eval "${command}" -eval 'halt().'

else
    erl -pa $(rebar3 path) -noshell -eval "application:ensure_all_started(aoc)"  -eval "${command}." -eval 'halt().'
fi

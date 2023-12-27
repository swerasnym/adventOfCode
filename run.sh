#! /bin/bash
# Evals a single erlang expression with the aoc code and config loaded

set -e
trap 'echo >&2 "Error - exited with status $? at line $LINENO:";
         pr -tn $0 | tail -n+$((LINENO - 3)) | head -n7' ERR

if [ "$#" != 1 ]; then
    echo Error: The takes ecactly one argument, the command to run. > /dev/stderr
    exit 1
fi
command="${1}"

working_dir=$(pwd)
script_dir=$(realpath $(dirname "${0}"))
if [ "${working_dir}" != "${script_dir}" ]; then
    echo Error: The script is not started from the root of the repo! > /dev/stderr
    exit 1
fi

config=""
if [ -f aoc.config ] ; then
    config="-config aoc.config"
elif [ "${1}" != "aoc_config:init()" ]; then
    echo Warning: Missing aoc.config, commands relying downloaded files will not work corectly. > /dev/stderr
fi

erl -pa $(rebar3 path) ${config} -noshell -eval 'application:ensure_all_started(aoc).' -eval "${command}" -eval 'halt().'

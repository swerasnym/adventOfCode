#! /bin/bash
set -e
trap 'echo >&2 "Error - exited with status $? at line $LINENO:";
         pr -tn $0 | tail -n+$((LINENO - 3)) | head -n7' ERR

working_dir=$(pwd)
script_dir=$(realpath $(dirname "${0}"))
if [ "${working_dir}" != "${script_dir}" ]; then
    echo Error: The script is not started from the root of the repo! > /dev/stderr
    exit 1
fi

rebar3 compile
./run.sh 'aoc_config:init()'

echo Installing pre-push hook
ln -sf ../../git_hooks/pre-push ./.git/hooks/pre-push
ls -l ./.git/hooks/pre-push

#!/bin/bash
set -e

rm -f build.lock

fswatch -o ghcid.log | xargs -n 1 bash -c 'killall week-exe; ./run.sh&'

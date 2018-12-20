#!/bin/bash
set -e

(killall ip-app || true) && ./run.sh&

rm -f build.lock

fswatch -o ghcid.log | xargs -n 1 bash -c 'killall ip-app; ./run.sh&'

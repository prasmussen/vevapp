#!/bin/bash
set -e

export LISTEN_PORT="8080"
export LISTEN_HOST="*4"
export IP_SOURCE="socket"
export STATIC_PATH="./static"

lockfile="build.lock"

if shlock -f "${lockfile}" -p $$; then
    echo "Building..."
    stack --silent build --ghc-options='-O0 -j +RTS -A128M -n2m -RTS'

    # Remove lock
    rm "${lockfile}"

    echo "Starting..."
    stack exec ip-app
else
    echo "Already building"
fi

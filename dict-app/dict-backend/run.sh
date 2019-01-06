#!/bin/bash

export DICTIONARIES_BASE_PATH="/Users/pii/dev/Projects/vevapp-files/dictionaries"
export STATIC_PATH="/Users/pii/dev/Projects/vevapp/dict-app/dict-frontend"
export MAX_ENTRIES="50"
export LISTEN_ADDR=":8080"

go run *.go

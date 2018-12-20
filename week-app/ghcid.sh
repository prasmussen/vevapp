#!/bin/bash

ghcid --command "stack ghci --main-is week:exe:week-exe" -T 'writeFile "ghcid.log" "ok"'

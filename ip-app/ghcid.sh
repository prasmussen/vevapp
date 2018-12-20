#!/bin/bash

ghcid --command "stack ghci --main-is ip-app:exe:ip-app" -T 'writeFile "ghcid.log" "ok"'

#!/bin/bash

elm make src/Main.elm --output tmp.js --optimize
uglifyjs tmp.js -o app.min.js --compress --mangle
rm tmp.js

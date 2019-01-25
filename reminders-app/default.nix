{ pkgs ? import <nixpkgs> {} }:
let
  src =
    ./.;

  cmd =
    ''
    appJsHash="$(sha1sum ${src}/app.min.js | awk '{print $1}')"

    mkdir -p $out
    cat ${src}/index.html | sed -e "s/app.js/app.js?hash=$appJsHash/" > $out/index.html
    cp ${src}/app.min.js $out/app.js
    '';
in
pkgs.runCommand "reminders-app" {} cmd

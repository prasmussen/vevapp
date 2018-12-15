{ pkgs ? import <nixpkgs> {} }:
let
  src =
    ./.;

  cmd =
    ''
    mkdir -p $out
    cp ${src}/index.html $out/
    cp ${src}/app.min.js $out/app.js
    '';
in
pkgs.runCommand "tax-app" {} cmd

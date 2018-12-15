{ pkgs ? import <nixpkgs> {} }:
let
  src =
    ./.;

  cmd =
    ''
    mkdir -p $out
    cp ${src}/index.html $out/
    '';
in
pkgs.runCommand "vevapp-index" {} cmd

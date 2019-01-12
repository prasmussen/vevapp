{ pkgs ? import <nixpkgs> {} }:
let
  src =
    ./src;

  cmd =
    ''
    export GOPATH="$(pwd)"
    go build -o $out/dict-backend ${src}/*.go
    '';
in
pkgs.runCommand "dict-backend" { buildInputs = [ pkgs.go ]; } cmd

{ pkgs, conf }:
let
  build = pkgs.writeShellScriptBin "build" ''
    set -euo pipefail
    nix-build nix/release.nix
  '';
  run = pkgs.writeShellScriptBin "run" ''
    set -euo pipefail
    result/bin/bill
  '';
in
[ build run ]

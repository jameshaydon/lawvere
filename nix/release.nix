{ system ? builtins.currentSystem }:
let
  pkgs = import ./. { inherit system; };
in
pkgs.lawvere

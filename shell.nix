{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix { inherit system; };
in
with pkgs; mkShell {
  buildInputs = [
    lawvere.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${lawvere.shell}/lib:$LD_LIBRARY_PATH
  '';
}

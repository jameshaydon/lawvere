{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

mkShell {
  name = "lawvere";
  buildInputs = [
    haskell.compiler.ghc8102
    stack
    zlib
  ];
}

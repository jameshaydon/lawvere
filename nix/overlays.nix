{ sources }:
[
  (_final: prev: {
    inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreFilter;
  })
  (_final: prev: {
    lawvere = (import ./packages.nix { pkgs = prev; });
  })
]

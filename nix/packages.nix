{ pkgs }: with pkgs;
let
  srcDir = ../.;

  # Don't rebuild when files in `filesIgnored`
  # or files under paths in `pathsIgnored` change
  filesIgnored = [
    "README.md"
  ];
  pathsIgnored = [
    "examples"
    "notes"
    "tools"
  ];

  filteredSrc = with builtins; lib.cleanSourceWith {
    src = srcDir;
    filter =
      let
        srcIgnored = gitignoreFilter srcDir; # in let binding to memoize
        relToRoot = lib.removePrefix (toString srcDir + "/");
      in
      path: type:
        srcIgnored path type
        && ! elem (baseNameOf path) filesIgnored
        && ! lib.any (d: lib.hasPrefix d (relToRoot path)) pathsIgnored;
  };

  haskellPackages = haskell.packages.ghc8103.override {
    overrides =
      let
        generated = haskell.lib.packagesFromDirectory {
          directory = ./packages;
        };
        manual = _hfinal: hprev: {
          lawvere = haskell.lib.overrideCabal hprev.lawvere (_drv: {
            src = filteredSrc;
          });
        };
      in
      lib.composeExtensions generated manual;
  };

  ghc = haskellPackages.ghc.withPackages (_ps:
    haskell.lib.getHaskellBuildInputs haskellPackages.lawvere
  );
in
{
  exe = haskellPackages.lawvere;

  shell = buildEnv {
    name = "lawvere-env";
    paths = [
      cabal-install
      cabal2nix
      ghc
      haskell-language-server
      haskellPackages.implicit-hie
      hlint
      hpack
      niv
      ormolu
    ];
  };
}

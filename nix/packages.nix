{ pkgs }: with pkgs;
let
  util = import ./util.nix {
    inherit pkgs;
    inherit (pkgs) lib gitignoreFilter;
  };

  conf = pkgs.lib.importTOML ../nixkell.toml;

  # Create our haskell from the choosen version of the default one
  ourHaskell = pkgs.haskell.packages.${("ghc" + util.removeDot conf.ghc)}.override {
    overrides =
      let
        depsFromDir = pkgs.haskell.lib.packagesFromDirectory {
          directory = ./packages;
        };
        manual = _hfinal: hprev: {
          lawvere = pkgs.haskell.lib.overrideCabal hprev.lawvere (_drv: {
            src = util.filterSrc ../. {
              ignoreFiles = conf.ignore.files;
              ignorePaths = conf.ignore.paths;
            };
          });
        };
      in
      pkgs.lib.composeExtensions depsFromDir manual;
  };

  # Include our package dependencies with ghc
  ghc = ourHaskell.ghc.withPackages (_ps:
    pkgs.haskell.lib.getHaskellBuildInputs ourHaskell.lawvere
  );

  tools = util.buildWith ourHaskell conf.env.tools [ "haskell-language-server" ];

  scripts = import ./scripts.nix { inherit pkgs conf; };

in
{
  bin = util.leanPkg ourHaskell.lawvere;

  shell = buildEnv {
    name = "lawvere-env";
    paths = [ ghc ] ++ tools ++ scripts;
  };
}

{ pkgs }: with pkgs;
let
  util = import ./util.nix { inherit (pkgs) haskell lib gitignoreFilter; };

  scripts = import ./scripts.nix { inherit pkgs conf; };

  conf = pkgs.lib.importTOML ../nixkell.toml;

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

  # Add our package to haskellPackages
  haskellPackages = pkgs.haskell.packages.${("ghc" + util.removeDot conf.env.ghc)}.override {
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
  ghc = haskellPackages.ghc.withPackages (_ps:
    pkgs.haskell.lib.getHaskellBuildInputs haskellPackages.lawvere
  );
in
{
  bin = util.leanPkg haskellPackages.lawvere;

  shell = buildEnv {
    name = "lawvere-env";
    paths = [ ghc ] ++ util.getFrom pkgs conf.env.packages ++ scripts;
  };
}

{ lib, gitignoreFilter }:
{
  removeDot = n: lib.replaceStrings [ "." ] [ "" ] n;

  getFrom = set: ns: map (n: lib.getAttrFromPath (lib.splitString "." n) set) ns;

  filterSrc = path: { ignoreFiles ? [ ], ignorePaths ? [ ] }:
    lib.cleanSourceWith {
      src = path;
      filter =
        let
          srcIgnored = gitignoreFilter path; # in let binding to memoize
          relToPath = lib.removePrefix (toString path + "/");
        in
        path: type:
          srcIgnored path type
          && ! builtins.elem (baseNameOf path) ignoreFiles
          && ! lib.any (d: lib.hasPrefix d (relToPath path)) ignorePaths;
    };
}

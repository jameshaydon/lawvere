{ mkDerivation, aeson, base, bytestring, commonmark
, commonmark-pandoc, containers, generic-lens, hpack, hspec
, hspec-discover, lens, lib, megaparsec, optparse-applicative
, pandoc-types, parser-combinators, prettyprinter, protolude, text
, transformers
}:
mkDerivation {
  pname = "lawvere";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring commonmark commonmark-pandoc containers
    generic-lens lens megaparsec optparse-applicative pandoc-types
    parser-combinators prettyprinter protolude text transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring commonmark commonmark-pandoc containers
    generic-lens lens megaparsec optparse-applicative pandoc-types
    parser-combinators prettyprinter protolude text transformers
  ];
  testHaskellDepends = [
    aeson base bytestring commonmark commonmark-pandoc containers
    generic-lens hspec lens megaparsec optparse-applicative
    pandoc-types parser-combinators prettyprinter protolude text
    transformers
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/jameshaydon/lawvere#readme";
  license = lib.licenses.mit;
}

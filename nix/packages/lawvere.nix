{ mkDerivation, aeson, ansi-terminal, base, bytestring, commonmark
, commonmark-pandoc, containers, generic-lens, haskeline, hpack
, hspec, hspec-discover, lens, lib, megaparsec
, optparse-applicative, pandoc-types, parser-combinators
, prettyprinter, prettyprinter-ansi-terminal, protolude
, terminal-size, text, transformers
}:
mkDerivation {
  pname = "lawvere";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base bytestring commonmark commonmark-pandoc
    containers generic-lens haskeline lens megaparsec
    optparse-applicative pandoc-types parser-combinators prettyprinter
    prettyprinter-ansi-terminal protolude terminal-size text
    transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson ansi-terminal base bytestring commonmark commonmark-pandoc
    containers generic-lens haskeline lens megaparsec
    optparse-applicative pandoc-types parser-combinators prettyprinter
    prettyprinter-ansi-terminal protolude terminal-size text
    transformers
  ];
  testHaskellDepends = [
    aeson ansi-terminal base bytestring commonmark commonmark-pandoc
    containers generic-lens haskeline hspec lens megaparsec
    optparse-applicative pandoc-types parser-combinators prettyprinter
    prettyprinter-ansi-terminal protolude terminal-size text
    transformers
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/jameshaydon/lawvere#readme";
  license = lib.licenses.mit;
}

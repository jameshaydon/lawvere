{ mkDerivation, aeson, base, bytestring, containers, generic-lens
, hpack, hspec, hspec-discover, lens, lib, megaparsec
, parser-combinators, prettyprinter, protolude, text, transformers
}:
mkDerivation {
  pname = "lawvere";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers generic-lens lens megaparsec
    parser-combinators prettyprinter protolude text transformers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers generic-lens lens megaparsec
    parser-combinators prettyprinter protolude text transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers generic-lens hspec lens megaparsec
    parser-combinators prettyprinter protolude text transformers
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/jameshaydon/lawvere#readme";
  license = lib.licenses.bsd3;
}

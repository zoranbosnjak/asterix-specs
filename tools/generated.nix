{ mkDerivation, aeson, aeson-pretty, base, bytestring, cryptonite
, formatting, lens, lib, megaparsec, optparse-applicative, text
, transformers, unordered-containers, with-utf8
}:
mkDerivation {
  pname = "aspecs";
  version = "1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring formatting lens megaparsec text
    transformers unordered-containers
  ];
  executableHaskellDepends = [
    base bytestring cryptonite optparse-applicative text with-utf8
  ];
  homepage = "https://zoranbosnjak.github.io/asterix-specs/";
  description = "Asterix specifications tools";
  license = lib.licenses.bsd3;
}

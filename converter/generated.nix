{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, directory
, filepath, formatting, megaparsec, optparse-applicative
, QuickCheck, stdenv, stm, template-haskell, text, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "converter";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring containers formatting megaparsec
    text transformers unordered-containers
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base base16-bytestring base64-bytestring
    bytestring clock containers directory filepath formatting
    megaparsec optparse-applicative QuickCheck stm template-haskell
    text time transformers unordered-containers
  ];
  description = "asterix specs converter";
  license = stdenv.lib.licenses.bsd3;
}

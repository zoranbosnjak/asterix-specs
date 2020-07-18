{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, cryptonite
, directory, filepath, formatting, megaparsec, optparse-applicative
, QuickCheck, stdenv, stm, template-haskell, text, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "converter";
  version = "0.4.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring containers cryptonite formatting
    megaparsec text transformers unordered-containers
  ];
  executableHaskellDepends = [
    aeson aeson-pretty base base16-bytestring base64-bytestring
    bytestring clock containers cryptonite directory filepath
    formatting megaparsec optparse-applicative QuickCheck stm
    template-haskell text time transformers unordered-containers
  ];
  description = "asterix specs converter";
  license = stdenv.lib.licenses.bsd3;
}

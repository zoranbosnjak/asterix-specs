{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, cryptonite
, directory, filepath, formatting, megaparsec, optparse-applicative
, QuickCheck, stdenv, stm, text, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "converter";
  version = "0.6.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base base16-bytestring base64-bytestring
    bytestring clock containers cryptonite directory filepath
    formatting megaparsec optparse-applicative QuickCheck stm text time
    transformers unordered-containers
  ];
  description = "asterix specs converter";
  license = stdenv.lib.licenses.bsd3;
}

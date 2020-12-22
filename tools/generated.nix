{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, cryptonite
, directory, filepath, formatting, megaparsec, optparse-applicative
, QuickCheck, stdenv, stm, text, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "aspecs";
  version = "0.7.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base base16-bytestring base64-bytestring
    bytestring clock containers cryptonite directory filepath
    formatting megaparsec optparse-applicative QuickCheck stm text time
    transformers unordered-containers
  ];
  description = "asterix specs tools";
  license = stdenv.lib.licenses.bsd3;
}

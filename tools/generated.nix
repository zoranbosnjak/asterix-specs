{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, cryptonite
, directory, filepath, formatting, lib, megaparsec
, optparse-applicative, QuickCheck, stm, text, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "aspecs";
  version = "0.10.0";
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
  license = lib.licenses.bsd3;
}

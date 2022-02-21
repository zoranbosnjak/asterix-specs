{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, base64-bytestring, bytestring, clock, containers, cryptonite
, directory, filepath, formatting, lib, megaparsec
, optparse-applicative, QuickCheck, stm, text, time, transformers
, unordered-containers, with-utf8
}:
mkDerivation {
  pname = "aspecs";
  version = "0.12.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base base16-bytestring base64-bytestring
    bytestring clock containers cryptonite directory filepath
    formatting megaparsec optparse-applicative QuickCheck stm text time
    transformers unordered-containers with-utf8
  ];
  description = "asterix specs tools";
  license = lib.licenses.bsd3;
}

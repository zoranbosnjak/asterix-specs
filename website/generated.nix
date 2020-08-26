{ mkDerivation, aeson, base, bytestring, directory, hakyll, stdenv
}:
mkDerivation {
  pname = "website";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring directory hakyll
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}

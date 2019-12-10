{ inShell ? null
, packages ? null
}:

let

  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else import packages { };

  version = builtins.replaceStrings ["\n"] [""] (builtins.readFile ./VERSION.txt);
  deps = with pkgs; [
    python3
    python3Packages.setuptools
  ];

  drv = pkgs.python3Packages.buildPythonApplication {
    pname = "asterix-renderer";
    version = version;
    src = ./.;
    propagatedBuildInputs = deps;
    preCheck = ''
    '';
    postInstall = ''
      cp README.md $out
    '';
    makeWrapperArgs = [
      "--set VERSION ${version}"
    ];
  } // { inherit env; };

  env = pkgs.stdenv.mkDerivation rec {
    name = "python-envorinment";
    buildInputs = deps;
    shellHook = ''
      export VERSION=${version};
      export PYTHONPATH=$(pwd):$PYTHONPATH
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then drv.env else drv


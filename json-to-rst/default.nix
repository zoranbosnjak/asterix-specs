{ packages ? null
, inShell ? null
}:

let

  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  version = builtins.replaceStrings ["\n"] [""] (builtins.readFile ./VERSION.txt);

  deps = with pkgs; [
    python3
    python3Packages.setuptools
  ];

  drv = pkgs.python3Packages.buildPythonApplication {
    pname = "json-to-rst-${version}";
    version = version;
    src = builtins.filterSource
      (path: type: type != "symlink" || baseNameOf path != "result")
      ./.;
    propagatedBuildInputs = deps;
    preCheck = ''
    '';
    postInstall = ''
    '';
    makeWrapperArgs = [
      "--set VERSION ${version}"
    ];
  } // { inherit env; };

  env = pkgs.stdenv.mkDerivation rec {
    name = "python-envorinment";
    buildInputs = deps;
    shellHook = ''
      export PYTHONPATH=$(pwd):$PYTHONPATH
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then drv.env else drv


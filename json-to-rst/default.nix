{ sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
, inShell ? null
}:

let
  version = builtins.replaceStrings ["\n"] [""] (builtins.readFile ./VERSION.txt);

  deps = with packages; [
    python3
    python3Packages.setuptools
  ];

  drv = packages.python3Packages.buildPythonApplication {
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

  env = packages.stdenv.mkDerivation rec {
    name = "python-envorinment";
    buildInputs = deps;
    shellHook = ''
      export PYTHONPATH=$(pwd):$PYTHONPATH
    '';
  };

in
  if inShell == false
    then drv
    else if packages.lib.inNixShell then drv.env else drv


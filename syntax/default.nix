{ inShell ? null
, packages ? null
}:

let

  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else import packages { };

  deps = with pkgs; [
    tk
  ];

  drv = pkgs.stdenv.mkDerivation rec {
    pname = "asterix-specs-syntax";
    version = "0.0";
    src = ./.;
    propagatedBuildInputs = deps;
    buildPhase = ''
    '';
    installPhase = ''
      mkdir -p $out
      echo "TODO" > $out/test
    '';
  } // { inherit env; };

  env = pkgs.stdenv.mkDerivation rec {
    name = "asterix-syntax-envorinment";
    buildInputs = deps;
    shellHook = ''
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then drv.env else drv


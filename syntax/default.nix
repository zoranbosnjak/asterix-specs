{ gitrev ? "devel"
, inShell ? null
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

      mkdir -p $out/syntax/sources
      for i in `ls *tcl | grep syntax`; do cp $i $out/syntax/sources; done

      mkdir -p $out/syntax/postscript
      for i in `ls *ps | grep syntax`; do cp $i $out/syntax/postscript; done
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


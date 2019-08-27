{ withHoogle ? false }:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = import (builtins.fetchGit nixpkgs) {};

  converter = import ./converter.nix { inherit withHoogle pkgs; };

in {

  conv = pkgs.stdenv.mkDerivation {
    name = "converter-environment";
    buildInputs = converter.env.nativeBuildInputs ++ [
      pkgs.haskellPackages.cabal-install
    ];
  };

  specs = pkgs.stdenv.mkDerivation {
    name = "specs-environment";
    buildInputs = [
      converter
      pkgs.python36Packages.sphinx
      pkgs.tk
    ];
  };
}


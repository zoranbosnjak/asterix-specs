{ packages ? null
, gitrev ? "devel"
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else import packages { };

  haskellPackages = pkgs.haskellPackages;
  drv = haskellPackages.callPackage ./generated.nix { };

  env = pkgs.stdenv.mkDerivation rec {
    name = "converter-devel-environment";
    buildInputs = drv.env.nativeBuildInputs ++ [
      pkgs.cabal2nix
      pkgs.ghcid
    ];
  };

in
  if pkgs.lib.inNixShell then env else drv



let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = import (builtins.fetchGit nixpkgs) {};

  haskellPackages = pkgs.haskellPackages;
  drv = haskellPackages.callPackage ./default.nix { };

in
  if pkgs.lib.inNixShell then drv.env else drv


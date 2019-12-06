{ packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else import packages { };

  haskellPackages = pkgs.haskellPackages;
  drv = haskellPackages.callPackage ./default.nix { };

in
  if pkgs.lib.inNixShell then drv.env else drv


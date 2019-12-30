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

in
  if pkgs.lib.inNixShell then drv.env else drv


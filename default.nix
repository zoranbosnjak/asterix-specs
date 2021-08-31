{ gitrev ? "devel"
, packages ? null
, inShell ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  drv = import ./website { inherit gitrev; packages = pkgs; inShell = false; };

  env = pkgs.stdenv.mkDerivation {
    name = "asterix-specs-environment";
    buildInputs = [];
    shellHook = ''
      echo "Run nix-shell inside individual sub-directory!"
      exit 1
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv


{ gitrev ? "devel"
, packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  shortGitrev = builtins.substring 0 7 gitrev;

  haskellPackages = pkgs.haskellPackages;

  drv = haskellPackages.callPackage ./generated.nix { };

  specs = import ../specs/default.nix { inherit gitrev; packages = pkgs; };

  syntax = import ../syntax/default.nix { inherit gitrev; packages = pkgs; inShell = false; };

  env = pkgs.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = drv.env.nativeBuildInputs ++ [
    ];
    shellHook = ''
      export SHORT_GITREV=${shortGitrev}
      export SPECS=${specs}
      export SYNTAX=${syntax}
    '';
  };

in env


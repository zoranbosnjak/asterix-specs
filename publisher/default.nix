{ inShell ? null
, packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else import packages { };

  renderer = import ../renderer/default.nix { inShell = false; inherit packages; };

  haskellPackages = pkgs.haskellPackages;
  converter = haskellPackages.callPackage ../converter/generated.nix { };

  deps = import ./deps.nix { inherit pkgs; } ++ [converter renderer];

  drv = pkgs.stdenv.mkDerivation rec {
    name = "publisher";
    src = ./.;
    propagatedBuildInputs = deps;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out
      echo "TODO..." > $out/testfile
    '';
  } // { inherit env; };

  env = pkgs.stdenv.mkDerivation rec {
    name = "publisher-envorinment";
    buildInputs = deps;
    shellHook = ''
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then drv.env else drv


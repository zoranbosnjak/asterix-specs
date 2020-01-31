{ packages ? null
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

  env = pkgs.stdenv.mkDerivation rec {
    name = "publisher-envorinment";
    buildInputs = deps;
    shellHook = ''
    '';
  };

in
  env


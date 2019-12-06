{ packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else import packages { };

  haskellPackages = pkgs.haskellPackages;
  converter = haskellPackages.callPackage ./converter/default.nix { };

  deps = import ./deps.nix { inherit pkgs; };

in
  pkgs.stdenv.mkDerivation {
    name = "asterix-specs-environment";
    buildInputs =
      converter.env.nativeBuildInputs
      ++ deps
      ++ [ pkgs.tree];
    shellHook = ''
    '';
  }


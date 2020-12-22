{ packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  tools = import ../tools/default.nix { packages = pkgs; inShell = false; };

  renderer = import ../renderer/default.nix { packages = pkgs; inShell = false; };

  deps = import ./deps.nix { inherit pkgs; } ++ [tools renderer];

  env = pkgs.stdenv.mkDerivation rec {
    name = "publisher-envorinment";
    buildInputs = deps;
    shellHook = ''
    '';
  };

in
  env


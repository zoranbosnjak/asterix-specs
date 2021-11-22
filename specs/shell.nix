{ packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  tools = import ../tools/default.nix { packages = pkgs; inShell = false; };
  json-to-rst = import ../json-to-rst/default.nix { packages = pkgs; inShell = false; };
  rst-to-pdf  = import ../rst-to-pdf/default.nix { packages = pkgs; inShell = false; };

  env = pkgs.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = [
      tools
      json-to-rst
      rst-to-pdf
      pkgs.pandoc
    ];
    shellHook = ''
      export PATH=${tools}/bin:$PATH
      export PATH=${json-to-rst}/bin:$PATH
      export PATH=${rst-to-pdf}/bin:$PATH
      export PATH=${pkgs.pandoc}/bin:$PATH
    '';
  };

in
  env


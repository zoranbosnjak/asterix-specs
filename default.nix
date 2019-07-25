{ }:

let

  config = { };

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { inherit config; };

  asterix-converter = pkgs.haskellPackages.callPackage ./generated.nix { };

in
  # if pkgs.lib.inNixShell then drv.env else drv

  pkgs.stdenv.mkDerivation {
    name = "asterix-specs-environment";
    buildInputs = [
      asterix-converter
      pkgs.python36Packages.sphinx
    ];
  }


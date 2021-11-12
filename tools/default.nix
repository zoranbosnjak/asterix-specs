{ packages ? null
, inShell ? null
, strip ? true
, static ? false    # build static binary
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  normalPkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  pkgs = if static == true
    then normalPkgs.pkgsMusl.pkgsMusl
    else normalPkgs;

  haskellPackages = with pkgs.haskell.lib; pkgs.haskellPackages.override {
    overrides = self: super: {
  };};

  drv1 = haskellPackages.callPackage ./generated.nix { };

  drv2 = drv1.overrideDerivation (oldAttrs: {
      src = builtins.filterSource
        (path: type: type != "symlink" || baseNameOf path != "result")
        ./.;
    }) // { inherit env; };

  drv = if static == true
    then drv2.overrideDerivation (oldAttrs: {
      configureFlags = [
        "--ghc-option=-optl=-static"
        "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
        "--extra-lib-dirs=${pkgs.zlib.static}/lib"
        "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ] ++ pkgs.lib.optionals (!strip) [
          "--disable-executable-stripping"
        ];
      })
    else drv2;

  env = pkgs.stdenv.mkDerivation rec {
    name = "aspecs-devel-environment";
    buildInputs = drv1.env.nativeBuildInputs ++ [
      pkgs.cabal2nix
      pkgs.ghcid
    ];
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv


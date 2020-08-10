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

  haskellPackages = if static == true
    then with pkgs.haskell.lib; pkgs.haskellPackages.override {
      overrides = self: super: {
        # Dependencies we need to patch
        hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
      };}
    else pkgs.haskellPackages;

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
    name = "converter-devel-environment";
    buildInputs = drv1.env.nativeBuildInputs ++ [
      pkgs.cabal2nix
      pkgs.ghcid
    ];
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv


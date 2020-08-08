{ compiler ? "ghc865", strip ? true }:

let

  nixpkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/0c960262d159d3a884dadc3d4e4b131557dad116.tar.gz) {};
  pkgs = nixpkgs.pkgsMusl.pkgsMusl;

  normalHaskellPackages = pkgs.haskell.packages.${compiler};

  haskellPackages = with pkgs.haskell.lib; normalHaskellPackages.override {
    overrides = self: super: {
      # Dependencies we need to patch
      hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
    };
  };

  converter = haskellPackages.callPackage ./generated.nix { };

  envVars = ''
    export SW_VERSION=$(cat *.cabal | grep "^version:" | awk '{print $2}')
  '';

  drv = converter.overrideDerivation (oldAttrs: {
    preBuild = envVars;
    src = builtins.filterSource
      (path: type: type != "symlink" || baseNameOf path != "result")
      ./.;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
      ] ++ pkgs.lib.optionals (!strip) [
        "--disable-executable-stripping"
      ];
   });

in
  if pkgs.lib.inNixShell then drv.env else drv


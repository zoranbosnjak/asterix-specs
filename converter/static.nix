{ packages ? null
, strip ? true
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  normalPkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  pkgs = normalPkgs.pkgsMusl;

  static-haskell-nix = builtins.fetchTarball {
    url    = "https://github.com/nh2/static-haskell-nix/archive/dbce18f4808d27f6a51ce31585078b49c86bd2b5.tar.gz";
    sha256 = "084hxnrywsgb73zr41argdkbhkxzm1rqn058pv1l4cp9g1gjr2rr";
  };

  staticHaskell = import "${static-haskell-nix}/survey/default.nix" {
    inherit normalPkgs;
  };

  haskellPackages = staticHaskell.haskellPackages;

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
  drv


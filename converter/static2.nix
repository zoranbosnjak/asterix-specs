let
  strip = true;
  compiler = "ghc865";

  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/nh2/nixpkgs/archive/83fd89945d355892e0e95747c9a2c519491c1600.tar.gz";
    sha256 = "05jybj66gdzdmjgawa2a72b6pf669rfb6pljhlc3lpyq6dlnw87d";
  };

  static-haskell-nix = builtins.fetchTarball {
    url    = "https://github.com/nh2/static-haskell-nix/archive/c360f2a15f6947b411ecbd7ebaea925f6dbd68df.tar.gz";
    sha256 = "0y6ppiagh6dbvdhhnrq572xnw2yzn6d0gcmajrfdgdfwhsl21g95";
  };

  overlay = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides = pkgsNew.haskell.lib.packageSourceOverrides {
              dhall = "1.29";
            };
          }
        );
      };
    };
  };

  normalPkgs = import nixpkgs {
    system = "x86_64-linux";
    config = { };
    overlays = [ overlay ];
  };

  pkgs = normalPkgs.pkgsMusl;

  staticHaskell = import "${static-haskell-nix}/survey/default.nix" {
    inherit compiler normalPkgs;
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


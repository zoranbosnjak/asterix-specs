{ sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
, inShell ? null
, strip ? true
, static ? false    # build static binary
}:

let
  pkgs = packages;

  haskellPackages = with pkgs.haskell.lib; pkgs.haskellPackages.override {
    overrides = self: super:
      let
        fixGHC = pkg:
          if static == true
          then
            pkg.override {
              enableRelocatedStaticLibs = true;
              enableShared = false;
              enableDwarf = false;
            }
          else
            pkg;
      in {
        ghc = fixGHC super.ghc;
        buildHaskellPackages = super.buildHaskellPackages.override (oldBuildHaskellPackages: {
          ghc = fixGHC oldBuildHaskellPackages.ghc;
        });
        # haskellPackage1 = self.callPackage ./nix/myPackage1.nix { };
        # haskellPackage2 = self.callPackage ./nix/myPackage2.nix { };
        # ...
  };};

  drv1 = if static == true
    then
      haskellPackages.callCabal2nixWithOptions "aspecs" ./. "-f nopandoc" { }
    else
      haskellPackages.callCabal2nix "aspecs" ./. { };

  drv2 = drv1.overrideDerivation (oldAttrs: {
    src = builtins.filterSource
      (path: type:
           (type != "directory" || baseNameOf path != "folds")
        && (type != "symlink" || baseNameOf path != "result"))
        ./.;
    }) // { inherit env; };

  drv = if static == true
    then drv2.overrideDerivation (oldAttrs: {
      configureFlags = [
          "--ghc-option=-split-sections"
          "--ghc-option=-optl=-static"
          "--disable-shared"
          "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
          # Static linking crud
          "--extra-lib-dirs=${pkgs.glibc.static}/lib"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
          # The ones below are due to GHC's runtime system
          # depending on libdw (DWARF info), which depends on
          # a bunch of compression algorithms.
          "--ghc-option=-optl=-lbz2"
          "--ghc-option=-optl=-lz"
          "--ghc-option=-optl=-lelf"
          "--ghc-option=-optl=-llzma"
          "--ghc-option=-optl=-lzstd"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
          "--extra-lib-dirs=${(pkgs.xz.override { enableStatic = true; }).out}/lib"
          "--extra-lib-dirs=${(pkgs.zstd.override { enableStatic = true; }).out}/lib"
          "--extra-lib-dirs=${(pkgs.bzip2.override { enableStatic = true; }).out}/lib"
          "--extra-lib-dirs=${(pkgs.elfutils.overrideAttrs (old: { dontDisableStatic= true; })).out}/lib"
          # double-conversion temporary patch
          # This is required on nix-packages 24.05 until this patch is merged
          # https://github.com/NixOS/nixpkgs/pull/322738
          "--extra-lib-dirs=${pkgs.double-conversion.overrideAttrs(_: { cmakeFlags = [ ]; })}/lib"
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
      haskellPackages.haskell-language-server
    ];
    shellHook = ''
      export LC_ALL=C.UTF-8
      export GHC_BASE=$(which ghc | cut -d '/' -f-4)
      export EXTENSIONS=$(cat .ghci | grep ":set -X" | awk '{print $2}' | xargs)
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv

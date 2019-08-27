{ gitrev ? "devel", withHoogle ? false, pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = if withHoogle
    then ( pkgs.haskellPackages.override {
      overrides = (self: super:
        {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        } ); } )
    else pkgs.haskellPackages;

  converter = haskellPackages.callPackage ./converter-generated.nix { };

  drv = converter.overrideDerivation (oldAttrs: {
      src = builtins.filterSource
        (path: type: type != "directory" || baseNameOf path != ".git")
        ./.;
      preBuild = ''
        export GIT_REV=${gitrev}
      '';
  }) // {
    env = converter.env.overrideAttrs (oldAttrs: {
        GIT_REV = "<unknown>";
    });
  };

in
  drv


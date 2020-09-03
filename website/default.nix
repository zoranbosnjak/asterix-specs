{ gitrev ? "devel"
, packages ? null
, inShell ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  shortGitrev = builtins.substring 0 7 gitrev;

  haskellPackages = pkgs.haskellPackages;

  converter = import ../converter/default.nix { packages = pkgs; inShell = false; };

  converterStatic = import ../converter/default.nix { packages = pkgs; inShell = false; static = true; };

  renderer = import ../renderer/default.nix { packages = pkgs; inShell = false; };

  site = haskellPackages.callPackage ./generated.nix { };

  specs = import ../specs/default.nix { inherit gitrev; packages = pkgs; };

  syntax = import ../syntax/default.nix { inherit gitrev; packages = pkgs; inShell = false; };

  envVars = ''
    export SHORT_GITREV=${shortGitrev}
    export SPECS=${specs}
    export SYNTAX=${syntax}
  '';

  env = pkgs.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = site.env.nativeBuildInputs ++ [
    ];
    shellHook = envVars;
  };

  drv = pkgs.stdenv.mkDerivation {
    name = "asterix-specs-website";
    preBuild = envVars;
    src = ./.;
    installPhase = ''
      mkdir -p $out

      mkdir -p $out/bin
      ln -s ${converter}/bin/converter $out/bin/converter
      ln -s ${converterStatic}/bin/converter $out/bin/converter-static
      ln -s ${renderer}/bin/render $out/bin/render

      cp ${specs}/manifest.json $out/manifest.json

      rm -f result
      rm -rf _site _cache
      ${site}/bin/site rebuild
      cp -a _site/* $out
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then env else drv


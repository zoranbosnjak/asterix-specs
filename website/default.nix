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

  tools = import ../tools/default.nix { packages = pkgs; inShell = false; };
  toolsStatic = import ../tools/default.nix { packages = pkgs; inShell = false; static = true; };

  json-to-rst = import ../json-to-rst/default.nix { packages = pkgs; inShell = false; };
  rst-to-pdf  = import ../rst-to-pdf/default.nix { packages = pkgs; inShell = false; };

  specs = import ./specs.nix { inherit gitrev; packages = pkgs;};

  site = haskellPackages.callPackage ./generated.nix { };

  syntax = import ../syntax/default.nix { inherit gitrev; packages = pkgs; inShell = false; };

  envVars = ''
    export SHORT_GITREV=${shortGitrev}
    export SPECS=${specs}
    export SYNTAX=${syntax}
    export TOOLS_VERSION=${toolsStatic.version}
    export TOOLS_SHA256=$(sha256sum ${toolsStatic}/bin/aspecs | cut -d " " -f1)
  '';

  env = pkgs.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = site.env.nativeBuildInputs ++ [
      json-to-rst
      rst-to-pdf
    ];
    shellHook = envVars;
  };

  drv = pkgs.stdenv.mkDerivation {
    name = "asterix-specs-website";
    preBuild = envVars;
    src = ./.;
    installPhase = ''
      mkdir -p $out
      echo ${gitrev} > $out/gitrev.txt

      mkdir -p $out/bin
      ln -s ${tools}/bin/aspecs $out/bin/aspecs
      ln -s ${toolsStatic}/bin/aspecs $out/bin/aspecs-static
      ln -s ${json-to-rst}/bin/json-to-rst $out/bin/json-to-rst
      ln -s ${rst-to-pdf}/bin/rst-to-pdf $out/bin/rst-to-pdf

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


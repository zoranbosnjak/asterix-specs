{ gitrev ? "devel"
, sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
, inShell ? null
}:

let
  shortGitrev = builtins.substring 0 7 gitrev;

  haskellPackages = packages.haskellPackages;

  tools = import ../tools/default.nix { inherit  packages; inShell = false; };
  toolsStatic = import ../tools/default.nix { inherit packages; inShell = false; static = true; };

  json-to-rst = import ../json-to-rst/default.nix { inherit packages; inShell = false; };
  rst-to-pdf  = import ../rst-to-pdf/default.nix { inherit packages; inShell = false; };

  specs = import ./specs.nix { inherit gitrev; inherit packages;};

  site = haskellPackages.callCabal2nix "website" ./. { };

  syntax = import ../syntax/default.nix { inherit gitrev; inherit packages; inShell = false; };

  envVars = ''
    export SHORT_GITREV=${shortGitrev}
    export SPECS=${specs}
    export SYNTAX=${syntax}
    export TOOLS_VERSION=${toolsStatic.version}
    export TOOLS_SHA256=$(sha256sum ${toolsStatic}/bin/aspecs | cut -d " " -f1)
  '';

  env = packages.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = site.env.nativeBuildInputs ++ [
      json-to-rst
      rst-to-pdf
    ];
    shellHook = envVars;
  };

  drv = packages.stdenv.mkDerivation {
    name = "asterix-specs-website";
    preBuild = envVars;
    src = ./.;
    buildInputs = [
      packages.zip
    ];
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

      cd $out
      tar -cvf  specs.tar specs
      tar -cvzf specs.tgz specs
      zip -r    specs.zip specs
    '';
  };

in
  if inShell == false
    then drv
    else if packages.lib.inNixShell then env else drv


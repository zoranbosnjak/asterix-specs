{ gitrev ? "devel"
, sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
, inShell ? null
}:

let
  shortGitrev = builtins.substring 0 7 gitrev;

  haskellPackages = packages.haskellPackages;

  aspecs = import ../aspecs/default.nix { inherit  packages; inShell = false; };
  aspecsStatic = import ../aspecs/default.nix { inherit packages; inShell = false; static = true; };

  to-pdf  = import ../to-pdf/default.nix { inherit packages; inShell = false; };

  specs = import ../specs/default.nix { inherit gitrev; inherit packages;};

  site = haskellPackages.callCabal2nix "website" ./. { };

  syntax = import ../syntax/default.nix { inherit gitrev; inherit packages; inShell = false; };

  envVars = ''
    export SHORT_GITREV=${shortGitrev}
    export SPECS=${specs}
    export SYNTAX=${syntax}
    export ASPECS_VERSION=${aspecsStatic.version}
    export ASPECS_SHA256=$(sha256sum ${aspecsStatic}/bin/aspecs | cut -d " " -f1)
  '';

  env = packages.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = site.env.nativeBuildInputs ++ [
      to-pdf
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
      ln -s ${aspecs}/bin/aspecs $out/bin/aspecs
      ln -s ${aspecsStatic}/bin/aspecs $out/bin/aspecs-static
      ln -s ${to-pdf}/bin/to-pdf $out/bin/to-pdf

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

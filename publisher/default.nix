{ inShell ? null
, packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else import packages { };

  renderer = import ../renderer/default.nix { inShell = false; inherit packages; };

  haskellPackages = pkgs.haskellPackages;
  converter = haskellPackages.callPackage ../converter/generated.nix { };

  tex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-basic latexmk cmap collection-fontsrecommended
      fncychap titlesec tabulary varwidth framed fancyvrb float parskip
      wrapfig upquote capt-of needspace etoolbox lastpage caption;
  };

  deps = [
    tex
    pkgs.python3
    pkgs.python3Packages.sphinx
    pkgs.inkscape
    converter
    renderer
  ];

  drv = pkgs.stdenv.mkDerivation rec {
    name = "publisher";
    src = ./.;
    propagatedBuildInputs = deps;
    installPhase = ''
      mkdir -p $out
      echo "TODO..." > $out/testfile
    '';
  } // { inherit env; };

  env = pkgs.stdenv.mkDerivation rec {
    name = "publisher-envorinment";
    buildInputs = deps;
    shellHook = ''
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then drv.env else drv


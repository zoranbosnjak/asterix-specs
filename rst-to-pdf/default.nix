{ packages ? null
, inShell ? null
}:

let

  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  tex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-basic collection-fontsrecommended
      unicode-math xcolor sectsty enumitem
      xetex;
    };

  deps = with pkgs; [
    tex
    pandoc
  ];

  drv = pkgs.stdenv.mkDerivation rec {
    name = "rst-to-pdf";
    src = builtins.filterSource
      (path: type: type != "symlink" || baseNameOf path != "result")
      ./.;
    propagatedBuildInputs = deps;
    nativeBuildInputs = [ pkgs.makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      cp $src/preamble.tex $out
      cp $src/rst-to-pdf $out/bin
      wrapProgram $out/bin/rst-to-pdf --prefix PATH ":" ${pkgs.pandoc}/bin --prefix PATH ":" ${tex}/bin;
    '';
  } // { inherit env; };

  env = pkgs.stdenv.mkDerivation rec {
    name = "pandoc-envorinment";
    buildInputs = deps;
    shellHook = ''
    '';
  };

in
  if inShell == false
    then drv
    else if pkgs.lib.inNixShell then drv.env else drv


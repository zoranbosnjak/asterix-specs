{ sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
, inShell ? null
}:

let
  tex = packages.texlive.combine {
    inherit (packages.texlive)
      scheme-basic collection-fontsrecommended
      unicode-math xcolor sectsty enumitem etoolbox
      xetex;
    };

  deps = with packages; [
    tex
    pandoc
  ];

  drv = packages.stdenv.mkDerivation rec {
    name = "to-pdf";
    src = builtins.filterSource
      (path: type: type != "symlink" || baseNameOf path != "result")
      ./.;
    propagatedBuildInputs = deps;
    nativeBuildInputs = [ packages.makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      cp $src/preamble.tex $out
      cp $src/to-pdf $out/bin
      wrapProgram $out/bin/to-pdf --prefix PATH ":" ${packages.pandoc}/bin --prefix PATH ":" ${tex}/bin;
    '';
  } // { inherit env; };

  env = packages.stdenv.mkDerivation rec {
    name = "pandoc-envorinment";
    buildInputs = deps;
    shellHook = ''
    '';
  };

in
  if inShell == false
    then drv
    else if packages.lib.inNixShell then drv.env else drv

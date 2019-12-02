{ pkgs
}:

let
  tex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      scheme-basic latexmk cmap collection-fontsrecommended
      fncychap titlesec tabulary varwidth framed fancyvrb float parskip
      wrapfig upquote capt-of needspace etoolbox lastpage caption;
  };

in
  [
    tex
    pkgs.python36
    pkgs.python36Packages.sphinx
    pkgs.tk
    pkgs.inkscape
  ]


{ pkgs
, matrix
}:

let
  deps = with pkgs; [
    python3
    python3Packages.jinja2
  ];

in
  pkgs.stdenv.mkDerivation {
    name = "asterix-specs-html";
    src = ./.;
    buildInputs = deps;
    installPhase = ''
      mkdir -p $out

      cat ${matrix} | python3 ./render_jinja.py index.html > $out/index.html

      cp style.css $out
    '';
  }


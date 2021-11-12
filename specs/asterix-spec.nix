{ gitrev ? "devel"
, packages ? null
, tools
, toolsStatic
, renderer
, catnumber
, spectype
, edition
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  shortGitrev = builtins.substring 0 7 gitrev;

  render_rst = ./rst.py;
  preamble = ./preamble.tex;

  name = "asterix-${catnumber}-${spectype}-${edition}";
  src = builtins.readFile (./. + "/cat" + catnumber + ("/" + spectype) + "-" + edition + ".ast" );
  orig = pkgs.writeText (name + "-source") src;

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

in with pkgs; runCommand name
  { propagatedBuildInputs = deps;
    FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [pkgs.dejavu_fonts]; };
  }
  ''
    echo ${name}
    mkdir $out

    # for each 'conversion' step, make sure that
    # 'tools' and 'toolsStatic' produce the same result
    # (diff will report an error if not)

    echo "validate, copy original"
    ${tools}/bin/aspecs validate -f ${orig} --ast
    cp ${orig} $out/definition.ast

    echo "create fingerprint"
    ${tools}/bin/aspecs checksum -f ${orig} --ast > $out/fingerprint
    ${toolsStatic}/bin/aspecs checksum -f ${orig} --ast > $out/fingerprint2
    diff $out/fingerprint $out/fingerprint2
    rm $out/fingerprint2

    echo "prettify to .txt"
    ${tools}/bin/aspecs convert -f ${orig} --ast --ast > $out/definition.txt
    ${toolsStatic}/bin/aspecs convert -f ${orig} --ast --ast > $out/definition.txt2
    diff $out/definition.txt $out/definition.txt2
    rm $out/definition.txt2
    ${tools}/bin/aspecs checksum -f $out/definition.txt --ast > $out/fingerprint2
    diff $out/fingerprint $out/fingerprint2
    rm $out/fingerprint2

    echo "convert to .json"
    ${tools}/bin/aspecs convert -f ${orig} --ast --json > $out/definition.json
    ${toolsStatic}/bin/aspecs convert -f ${orig} --ast --json > $out/definition.json2
    diff $out/definition.json $out/definition.json2
    rm $out/definition.json2
    ${tools}/bin/aspecs checksum -f $out/definition.json --json > $out/fingerprint2
    diff $out/fingerprint $out/fingerprint2
    rm $out/fingerprint2

    echo "render to .rst"
    ${renderer}/bin/render --script ${render_rst} render $out/definition.json > $out/definition.rst

    echo "generate pdf"
    pandoc "$out/definition.rst" \
      -f rst \
      --include-in-header ${preamble} \
      -V linkcolor:blue \
      -V geometry:a4paper \
      -V geometry:margin=2cm \
      --pdf-engine=xelatex \
      -o "$out/definition.pdf"
  ''


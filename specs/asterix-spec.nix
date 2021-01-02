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

  render_rst = ../publisher/rst.py;

  publisher = ../publisher;
  publisher_deps = import ../publisher/deps.nix { inherit pkgs; };

  name = "asterix-${catnumber}-${spectype}-${edition}";
  src = builtins.readFile (./. + "/cat" + catnumber + ("/" + spectype) + "-" + edition + ".ast" );
  orig = pkgs.writeText (name + "-source") src;

in with pkgs; runCommand name
  { propagatedBuildInputs = publisher_deps;
    FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = pkgs.texlive.dejavu.pkgs; };
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

    echo "convert to .xml"
    ${tools}/bin/aspecs convert -f ${orig} --ast --xml > $out/definition.xml
    ${toolsStatic}/bin/aspecs convert -f ${orig} --ast --xml > $out/definition.xml2
    diff $out/definition.xml $out/definition.xml2
    rm $out/definition.xml2

    echo "render to .rst"
    ${renderer}/bin/render --script ${render_rst} render $out/definition.json > $out/definition.rst

    echo "generate html and pdf version"
    cp -a ${publisher}/* .
    cp $out/definition.json ./specs.json
    cp $out/definition.rst ./specs.rst

    chmod 755 _build
    make html
    make latexpdf
    cp -a _build/html/. $out/definition.html
    cp _build/latex/test.pdf $out/definition.pdf
  ''


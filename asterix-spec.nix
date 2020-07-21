{ gitrev ? "devel"
, packages ? null
, converter
, renderer
, catnumber
, spectype
, edition
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  shortGitrev = builtins.substring 0 7 gitrev;

  render_rst = ./publisher/rst.py;

  publisher = ./publisher;
  publisher_deps = import ./publisher/deps.nix { inherit pkgs; };

  name = "asterix-${catnumber}-${spectype}-${edition}";
  src = builtins.readFile (./specs + "/cat" + catnumber + ("/" + spectype) + "-" + edition + ".ast" );
  orig = pkgs.writeText (name + "-source") src;

in with pkgs; runCommand name
  { propagatedBuildInputs = publisher_deps;
    FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = pkgs.texlive.dejavu.pkgs; };
  }
  ''
    echo ${name}
    mkdir $out

    echo "validate, copy original"
    ${converter}/bin/converter -f ${orig} --ast --validate
    cp ${orig} $out/definition.ast

    echo "create fingerprint"
    ${converter}/bin/converter -f ${orig} --ast --sha1 > $out/fingerprint

    echo "pretify to .txt"
    ${converter}/bin/converter -f ${orig} --ast --ast > $out/definition.txt

    echo "convert to .json"
    ${converter}/bin/converter -f ${orig} --ast --json > $out/definition.json

    echo "convert to .xml"
    ${converter}/bin/converter -f ${orig} --ast --xml > $out/definition.xml

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


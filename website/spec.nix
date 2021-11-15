{ gitrev ? "devel"
, packages ? null
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

  name = "asterix-${catnumber}-${spectype}-${edition}";
  src = builtins.readFile (../specs/. + "/cat" + catnumber + ("/" + spectype) + "-" + edition + ".ast" );
  orig = pkgs.writeText (name + "-source") src;

  tools = import ../tools/default.nix { packages = pkgs; inShell = false; };
  toolsStatic = import ../tools/default.nix { packages = pkgs; inShell = false; static = true; };

  json-to-rst = import ../json-to-rst/default.nix { packages = pkgs; inShell = false; };
  rst-to-pdf = import ../rst-to-pdf/default.nix { packages = pkgs; inShell = false; };

  deps = with pkgs; [
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

    echo "prettify ast -> txt"
    ${tools}/bin/aspecs convert -f ${orig} --ast --ast > $out/definition.txt
    ${toolsStatic}/bin/aspecs convert -f ${orig} --ast --ast > $out/definition.txt2
    diff $out/definition.txt $out/definition.txt2
    rm $out/definition.txt2
    ${tools}/bin/aspecs checksum -f $out/definition.txt --ast > $out/fingerprint2
    diff $out/fingerprint $out/fingerprint2
    rm $out/fingerprint2

    echo "convert ast -> json"
    ${tools}/bin/aspecs convert -f ${orig} --ast --json > $out/definition.json
    ${toolsStatic}/bin/aspecs convert -f ${orig} --ast --json > $out/definition.json2
    diff $out/definition.json $out/definition.json2
    rm $out/definition.json2
    ${tools}/bin/aspecs checksum -f $out/definition.json --json > $out/fingerprint2
    diff $out/fingerprint $out/fingerprint2
    rm $out/fingerprint2

    echo "render json -> rst"
    ${json-to-rst}/bin/json-to-rst $out/definition.json > $out/definition.rst

    echo "convert rst -> pdf"
    ${rst-to-pdf}/bin/rst-to-pdf $out/definition.rst $out/definition.pdf
  ''


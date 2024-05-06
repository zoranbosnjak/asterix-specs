{ gitrev ? "devel"
, sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
, catnumber
, spectype
, edition
}:

let
  to-pdf = import ../to-pdf/default.nix { inherit packages; inShell = false; };
  shortGitrev = builtins.substring 0 7 gitrev;

  name = "asterix-${catnumber}-${spectype}-${edition}";
  src = builtins.readFile
    (./. + "/cat" + catnumber + ("/" + spectype) + "-" + edition + ".ast" );
  orig = packages.writeText (name + "-source") src;

  aspecs = import ../aspecs/default.nix { inherit packages; inShell = false; };

  style1 = packages.writeText "style1" (builtins.readFile (../style/default.css));
  style2 = packages.writeText "style2" (builtins.readFile (../style/syntax.css));

  deps = with packages; [
    pandoc
    to-pdf
  ];

in with packages; runCommand name
  { propagatedBuildInputs = deps;
    FONTCONFIG_FILE = packages.makeFontsConf
      { fontDirectories = [packages.dejavu_fonts]; };
  }
  ''
    echo ${name}
    mkdir $out

    echo "validate"
    ${aspecs}/bin/aspecs validate --input-ast ${orig}
    cp ${orig} $out/definition.ast

    echo "create fingerprint"
    ${aspecs}/bin/aspecs checksum --input-ast --sha256 ${orig} > $out/checksum.sha256

    echo "create txt"
    ${aspecs}/bin/aspecs convert --input-ast --output-ast ${orig} \
        > $out/definition.txt

    echo "create pandoc native"
    ${aspecs}/bin/aspecs pandoc --input-ast ${orig} \
        > $out/definition.pandoc.native

    echo "create html"
    pandoc --metadata title="asterix specification" \
        -f native -t html $out/definition.pandoc.native \
        -o $out/definition.html --embed-resources --standalone \
        --css=${style1} --css=${style2}

    echo "create pdf"
    to-pdf $out/definition.pandoc.native $out/definition.pdf

    echo "create internal"
    ${aspecs}/bin/aspecs convert --input-ast --output-internal ${orig} \
        > $out/definition.internal
    ${aspecs}/bin/aspecs checksum --sha256 \
        --input-internal $out/definition.internal > $out/definition.internal.cks
    diff $out/checksum.sha256 $out/definition.internal.cks
    rm $out/definition.internal.cks

    echo "create json"
    ${aspecs}/bin/aspecs convert --input-ast --output-json ${orig} \
        > $out/definition.json
    ${aspecs}/bin/aspecs checksum --sha256 \
        --input-json $out/definition.json > $out/definition.json.cks
    diff $out/checksum.sha256 $out/definition.json.cks
    rm $out/definition.json.cks
  ''

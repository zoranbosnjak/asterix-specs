{ gitrev ? "devel"
, sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
, catnumber
, spectype
, edition
}:

let
  shortGitrev = builtins.substring 0 7 gitrev;

  name = "asterix-${catnumber}-${spectype}-${edition}";
  src = builtins.readFile
    (./. + "/cat" + catnumber + ("/" + spectype) + "-" + edition + ".ast" );
  orig = packages.writeText (name + "-source") src;

  aspecs = import ../aspecs/default.nix { inherit packages; inShell = false; };

  deps = with packages; [
    pandoc
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
    ${aspecs}/bin/aspecs checksum --input-ast --sha512 ${orig} > $out/checksum.sha512

    #echo "create txt"
    ${aspecs}/bin/aspecs convert --input-ast --output-ast ${orig} \
        > $out/definition.txt

    #echo "create json"
    ${aspecs}/bin/aspecs convert --input-ast --output-json ${orig} \
        > $out/definition.json

    #echo "create pandoc native"
    ${aspecs}/bin/aspecs pandoc --input-ast ${orig} \
        > $out/definition.pandoc.native

    #echo "create html"

    #echo "create pdf"
  ''

{ gitrev ? "devel"
, packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else import packages { };

  syntax = import ./syntax/default.nix { inherit gitrev; };

  converter = import ./converter/default.nix { inherit gitrev; };

  renderer = import ./renderer/default.nix { inShell = false; };

  deps = import ./publisher/deps.nix { inherit pkgs; };

  drv = pkgs.stdenv.mkDerivation {
    name = "asterix-specs";
    propagatedBuildInputs = deps;
    src = builtins.filterSource
      (path: type: type != "directory" || baseNameOf path != ".git")
      ./.;
    installPhase = ''
      mkdir -p $out
      mkdir -p $out/bin

      ln -s ${syntax}/syntax $out/syntax
      ln -s ${converter}/bin/converter $out/bin/converter
      ln -s ${renderer}/bin/render $out/bin/render

      cp publisher/style.css $out

      ix=$out/index.html
      echo "<!DOCTYPE html>" >> $ix
      echo "<head><link href="style.css" rel="stylesheet" type="text/css"></head>" >> $ix
      echo "<html>" >> $ix
      echo "<body>" >> $ix

      echo "Asterix specifications generated from repository revision <code>#${gitrev}</code>." >> $ix

      mkdir -p $out/specs
      for level1 in $(find specs/* -maxdepth 1 -type d); do
        cat=$(basename $level1)
        mkdir -p $out/specs/$cat

        echo "<h2>$cat</h2>" >> $ix
        echo "<ul>" >> $ix

        for level2 in $(find $level1 -type f | grep ast | cut -c 2- | sort -t. -k1,1n -k2,2n); do
          edition=$(basename v$level2 .ast)
          echo $cat $edition

          dst=$out/specs/$cat/$edition
          mkdir $dst

          orig=specs/$cat/$edition.ast
          base=$dst/$cat-$edition

          echo "validate, copy original"
          ${converter}/bin/converter --validate -f $orig
          cp $orig $base.ast

          echo "convert to .json"
          ${converter}/bin/converter --json -f $orig > $base.json

          echo "pretify"
          ${renderer}/bin/render --script renderer/formats/ast.py render $base.json > $base.txt

          echo "convert again, check"
          # convert back to ast, then to json again, expect the same .json file
          ${converter}/bin/converter --json -f $base.txt > $base.json2
          diff -q $base.json $base.json2
          rm $base.json2

          echo "render to .rst"
          ${renderer}/bin/render --script publisher/rst.py render $base.json > $base.rst

          current=$PWD
          echo "generate html and pdf version"
          rm -rf $TMP/publisher
          cp -a publisher $TMP
          cp $base.json $TMP/publisher/specs.json
          cp $base.rst $TMP/publisher/specs.rst
          cd $TMP/publisher
          make html
          make latexpdf
          mkdir $base.html
          cp -a $TMP/publisher/_build/html/. $base.html
          cp $TMP/publisher/_build/latex/test.pdf $base.pdf
          cd $current

          echo "<li> $edition" >> $ix
          ref=specs/$cat/$edition/$cat-$edition
          echo "<a href=\"$ref.ast\">[ast]</a>" >> $ix
          echo "<a href=\"$ref.txt\">[txt]</a>" >> $ix
          echo "<a href=\"$ref.json\">[json]</a>" >> $ix
          echo "<a href=\"$ref.rst\">[rst]</a>" >> $ix
          echo "<a href=\"$ref.pdf\">[pdf]</a>" >> $ix
          echo "<a href=\"$ref.html/specs.html\">[html]</a>" >> $ix
          echo "</li>" >> $ix

        done
        echo "</ul>" >> $ix

      done

	  echo "<hr>" >> $ix
	  echo "Format description:" >> $ix
	  echo "<ul>" >> $ix
	  echo "    <li><code>ast</code> source format</li>" >> $ix
	  echo "    <li><code>txt</code> reformated (same as source), generated from <code>json</code></li>" >> $ix
	  echo "    <li><code>json</code> representation, generated from <code>ast</code></li>" >> $ix
	  echo "    <li><code>rst</code> documentation format, generated from <code>json</code></li>" >> $ix
	  echo "    <li><code>pdf</code> documentation, generated from <code>rst</code></li>" >> $ix
	  echo "    <li><code>html</code> documentation, generated from <code>rst</code></li>" >> $ix
	  echo "</ul>" >> $ix

      echo "</body>" >> $ix
      echo "</html>" >> $ix

    '';
  } // { inherit env; };

  env = pkgs.stdenv.mkDerivation {
    name = "asterix-specs-environment";
    buildInputs = [];
      #converter.env.nativeBuildInputs
      #++ renderer.env.nativeBuildInputs;
    shellHook = ''
      echo "Run nix-shell inside individual sub-directory!"
      exit 1
    '';
  };

in
    if pkgs.lib.inNixShell then drv.env else drv


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

      mkdir -p $out/specs
      for level1 in $(find specs/* -maxdepth 1 -type d); do
        cat=$(basename $level1)
        mkdir -p $out/specs/$cat
        for level2 in $(find $level1 -type f | grep ast); do
          edition=$(basename $level2)
          json=$out/specs/$cat/$edition.json
          rst=$out/specs/$cat/$edition.rst
          html=$out/specs/$cat/$edition.html
          pdf=$out/specs/$cat/$edition.pdf
          echo $level2

          echo "validate, copy original"
          ${converter}/bin/converter --validate -f $level2
          cp $level2 $out/specs/$cat/$edition

          echo "convert to .json"
          ${converter}/bin/converter --json -f $level2 > $json

          echo "pretify, convert again..."
          # convert back to ast, then to json again, expect the same .json file
          rm -rf $TMP/check
          mkdir $TMP/check
          ${renderer}/bin/render --script renderer/formats/ast.py render $json > $TMP/check/ast2.ast
          ${converter}/bin/converter --json -f $TMP/check/ast2.ast > $TMP/check/json2.json
          diff -q $json $TMP/check/json2.json

          echo "render to .rst"
          ${renderer}/bin/render --script publisher/rst.py render $json > $rst

          echo "generate html and pdf version"
          current=$PWD
          rm -rf $TMP/publisher
          cp -a publisher $TMP
          cat $json > $TMP/publisher/specs.json
          cat $rst > $TMP/publisher/specs.rst
          cd $TMP/publisher
          make html
          make latexpdf
          cp -a $TMP/publisher/_build/html $html
          cp -a $TMP/publisher/_build/latex/test.pdf $pdf
          cd $current
        done
      done
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


{ gitrev ? "devel"
, packages ? null
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

  shortGitrev = builtins.substring 0 7 gitrev;

  syntax = import ./syntax/default.nix { inherit gitrev; packages = pkgs; inShell = false; };

  converter = import ./converter/default.nix { packages = pkgs; inShell = false; };

  converterStatic = import ./converter/default.nix { packages = pkgs; inShell = false; static = true; };

  renderer = import ./renderer/default.nix { packages = pkgs; inShell = false; };

  cats =
    let
      toCatNumber = s:
        let
          x = builtins.split "cat" s;
        in if (builtins.length x) == 3
          then builtins.head (builtins.tail (builtins.tail x))
          else null;

      isCat = key: val:
        (val == "directory" && toCatNumber key != null);
    in map toCatNumber (builtins.attrNames (pkgs.lib.attrsets.filterAttrs isCat (builtins.readDir ./specs)));

  values = lst: builtins.filter (x: x != null) lst;

  isRegular = key: val: (val == "regular");

  listing = catnum: builtins.attrNames (pkgs.lib.attrsets.filterAttrs isRegular (builtins.readDir (./specs + "/cat" + catnum)));

  findAst = x: s:
    let m = builtins.match (x + "-(.*).ast") s;
    in if m == null
      then null
      else builtins.head m;

  catsUnder = catnum: values (map (findAst "cat") (listing catnum));

  refsUnder = catnum: values (map (findAst "ref") (listing catnum));

  level1 = catnum:
    let
      asterix-spec = catnumber: spectype: edition:
        import ./asterix-spec.nix { inherit gitrev; packages = pkgs; inherit converter renderer catnumber spectype edition;};

      linkCats =
        let linkCat = ed: "\"" + ed + " " + asterix-spec catnum "cat" ed + "\"";
        in toString (map linkCat (catsUnder catnum));

      linkRefs =
        let linkRef = ed: "\"" + ed + " " + asterix-spec catnum "ref" ed + "\"";
        in toString (map linkRef (refsUnder catnum));

    in
      pkgs.stdenv.mkDerivation {
        name = "asterix-category-" + catnum;
        src = builtins.filterSource
          (path: type:
            (type != "directory" || baseNameOf path != ".git")
            && (type != "symlink" || baseNameOf path != "result"))
          ./specs;

        installPhase = ''
          mkdir -p $out
          echo ${catnum} > $out/category

          mkdir -p $out/cats
          for i in ${linkCats}; do
            set -- $i
            ln -s $2 $out/cats/cat$1
          done

          mkdir -p $out/refs
          for i in ${linkRefs}; do
            set -- $i
            ln -s $2 $out/refs/ref$1
          done
        '';
      };

  linkCategories =
    let linkCategory = cat: "\"" + cat + " " + level1 cat + "\"";
    in toString (map linkCategory cats);

  matrix =
    let obj =
      { inherit shortGitrev;
        categories = map (catnum: { category = catnum; cats = catsUnder catnum; refs = refsUnder catnum;}) cats;
      };
    in  pkgs.writeText "asterix-specs-matrix" ''
      ${builtins.toJSON obj}
    '';

  html = import ./html {inherit pkgs matrix;};

  drv = pkgs.stdenv.mkDerivation {
    name = "asterix-specs";
    src = builtins.filterSource
      (path: type:
          (type != "directory" || baseNameOf path != ".git")
       && (type != "symlink" || baseNameOf path != "result"))
      ./specs;

    installPhase = ''
      mkdir -p $out

      ln -s ${syntax} $out/syntax

      mkdir -p $out/bin
      ln -s ${converter}/bin/converter $out/bin/converter
      ln -s ${converterStatic}/bin/converter $out/bin/converter-static
      ln -s ${renderer}/bin/render $out/bin/render

      mkdir -p $out/specs
      for i in ${linkCategories}; do
        set -- $i
        ln -s $2 $out/specs/cat$1
      done

      cp ${html}/index.html $out
      cp ${html}/style.css $out
    '';
  } // { inherit env; };

  env = pkgs.stdenv.mkDerivation {
    name = "asterix-specs-environment";
    buildInputs = [];
    shellHook = ''
      echo "Run nix-shell inside individual sub-directory!"
      exit 1
    '';
  };

in
  if pkgs.lib.inNixShell then drv.env else drv


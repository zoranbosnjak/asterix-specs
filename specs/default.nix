{ gitrev ? "devel"
, packages ? null
, tools ? import ../tools/default.nix { inShell = false; }
, toolsStatic ? import ../tools/default.nix { inShell = false; static = true; }
, renderer ? import ../renderer/default.nix { inShell = false; }
}:

let
  nixpkgs = builtins.fromJSON (builtins.readFile ../nixpkgs.json);
  pkgs = if packages == null
    then import (builtins.fetchGit nixpkgs) { }
    else packages;

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
    in map toCatNumber (builtins.attrNames (pkgs.lib.attrsets.filterAttrs isCat (builtins.readDir ./.)));

  values = lst: builtins.filter (x: x != null) lst;

  isRegular = key: val: (val == "regular");

  listing = catnum: builtins.attrNames (pkgs.lib.attrsets.filterAttrs isRegular (builtins.readDir (./. + "/cat" + catnum)));

  findAst = x: s:
    let m = builtins.match (x + "-(.*).ast") s;
    in if m == null
      then null
      else builtins.head m;

  sortEditions = lst:
    let compareEditions = a: b: builtins.compareVersions a b == 1;
    in builtins.sort compareEditions lst;

  catsUnder = catnum: sortEditions (values (map (findAst "cat") (listing catnum)));

  refsUnder = catnum: sortEditions (values (map (findAst "ref") (listing catnum)));

  manifest =
    let
      obj = map (catnum: { category = catnum; cats = catsUnder catnum; refs = refsUnder catnum;}) cats;
    in
      pkgs.writeText "asterix-specs-manifest" ''
        ${builtins.toJSON obj}
      '';

  level1 = catnum:
    let
      asterix-spec = catnumber: spectype: edition:
        import ./asterix-spec.nix { inherit gitrev; packages = pkgs; inherit tools toolsStatic renderer catnumber spectype edition;};

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
          ./.;

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

  drv = pkgs.stdenv.mkDerivation {
    name = "asterix-specs";
    src = builtins.filterSource
      (path: type:
          (type != "directory" || baseNameOf path != ".git")
       && (type != "symlink" || baseNameOf path != "result"))
      ./.;

    installPhase = ''
      mkdir -p $out

      cat ${manifest} | ${pkgs.jq}/bin/jq > $out/manifest.json

      mkdir -p $out/specs
      for i in ${linkCategories}; do
        set -- $i
        ln -s $2 $out/specs/cat$1
      done
    '';
  };

in
  drv


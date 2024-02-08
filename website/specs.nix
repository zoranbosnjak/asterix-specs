{ gitrev ? "devel"
, sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
}:

let
  tools = import ../tools/default.nix { inherit packages; inShell = false; };

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
    in map toCatNumber (builtins.attrNames (packages.lib.attrsets.filterAttrs isCat (builtins.readDir ../specs)));

  values = lst: builtins.filter (x: x != null) lst;

  isRegular = key: val: (val == "regular");

  listing = catnum: builtins.attrNames (packages.lib.attrsets.filterAttrs isRegular (builtins.readDir (../specs/. + "/cat" + catnum)));

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
      packages.writeText "asterix-specs-manifest" ''
        ${builtins.toJSON obj}
      '';

  level1 = catnum:
    let
      asterix-spec = catnumber: spectype: edition:
        import ./spec.nix { inherit gitrev; inherit packages; inherit catnumber spectype edition;};

      linkCats =
        let linkCat = ed: "\"" + ed + " " + asterix-spec catnum "cat" ed + "\"";
        in toString (map linkCat (catsUnder catnum));

      linkRefs =
        let linkRef = ed: "\"" + ed + " " + asterix-spec catnum "ref" ed + "\"";
        in toString (map linkRef (refsUnder catnum));

    in
      packages.stdenv.mkDerivation {
        name = "asterix-category-" + catnum;
        src = builtins.filterSource
          (path: type:
            (type != "directory" || baseNameOf path != ".git")
            && (type != "symlink" || baseNameOf path != "result"))
          ./.;

        installPhase = ''
          mkdir -p $out

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

  drv = packages.stdenv.mkDerivation {
    name = "asterix-specs";
    src = builtins.filterSource
      (path: type:
          (type != "directory" || baseNameOf path != ".git")
       && (type != "symlink" || baseNameOf path != "result"))
      ../specs;

    installPhase = ''
      mkdir -p $out

      cat ${manifest} | ${packages.jq}/bin/jq > $out/manifest.json

      mkdir -p $out/specs
      for i in ${linkCategories}; do
        set -- $i
        ln -s $2 $out/specs/cat$1
      done

      # test specs validation
      for i in $(find ../specs/test/*ast); do
          echo "validating test spec: $i"
          ${tools}/bin/aspecs validate -f $i --ast --warnings
      done
    '';
  };

in
  drv

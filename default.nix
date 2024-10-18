{ gitrev ? "devel"
, gitdate ? "YYYY-MM-DD"
, sources ? import ./nix/sources.nix
, packages ? import sources.nixpkgs {}
, inShell ? null
}:

let
  drv = import ./website { inherit gitrev; inherit gitdate; inherit packages; inShell = false; };

  env = packages.stdenv.mkDerivation {
    name = "asterix-specs-environment";
    buildInputs = [];
    shellHook = ''
      echo "Run nix-shell inside individual sub-directory!"
      exit 1
    '';
  };

in
  if inShell == false
    then drv
    else if packages.lib.inNixShell then env else drv

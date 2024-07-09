{ sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
}:

let
  aspecs = import ../aspecs/default.nix { inherit packages; inShell = false; };
  to-pdf  = import ../to-pdf/default.nix { inherit packages; inShell = false; };

  env = packages.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = [
      aspecs
      to-pdf
      packages.pandoc
    ];
    shellHook = ''
      export PATH=${aspecs}/bin:$PATH
      export PATH=${to-pdf}/bin:$PATH
      export PATH=${packages.pandoc}/bin:$PATH
    '';
  };

in
  env

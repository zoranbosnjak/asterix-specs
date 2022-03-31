{ sources ? import ../nix/sources.nix
, packages ? import sources.nixpkgs {}
}:

let
  tools = import ../tools/default.nix { inherit packages; inShell = false; };
  json-to-rst = import ../json-to-rst/default.nix { inherit packages; inShell = false; };
  rst-to-pdf  = import ../rst-to-pdf/default.nix { inherint packages; inShell = false; };

  env = packages.stdenv.mkDerivation rec {
    name = "website-devel-environment";
    buildInputs = [
      tools
      json-to-rst
      rst-to-pdf
      packages.pandoc
    ];
    shellHook = ''
      export PATH=${tools}/bin:$PATH
      export PATH=${json-to-rst}/bin:$PATH
      export PATH=${rst-to-pdf}/bin:$PATH
      export PATH=${packages.pandoc}/bin:$PATH
    '';
  };

in
  env


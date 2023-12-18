{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, aspecsRef ? builtins.fromJSON (builtins.readFile ./nix/aspecs.json)
}:

let

  aspecsDir = pkgs.fetchgit {
    url = aspecsRef.url;
    rev = aspecsRef.rev;
    sha256 = aspecsRef.sha256;
  };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld: rec {
      aspecs = haskellPackagesNew.callPackage "${aspecsDir}/tools" {packages=pkgs; inShell=false;};
    };
  };

  haskellDeps = ps: with ps; [
    aspecs
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    pkgs.ghcid
  ];

in pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
  shellHook = ''
      export EXTENSIONS=$(cat .ghci | grep ":set -X" | awk '{print $2}' | xargs)
  '';
}

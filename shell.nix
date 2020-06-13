{ nixpkgs ? import ./nix/nix.nix }:
let
  pkgs = import nixpkgs {};
  hkg = pkgs.haskellPackages;
  ghc = hkg.ghcWithPackages (hpkgs: with hpkgs; [ ghcide
                                                  cabal-install
                                                  zlib
                                                ]);
in
  with pkgs;
  mkShell {
    buildInputs = [ binutils
                    ghc
                    git
                    gnumake
                    gnumake
                    hkg.fswatcher
                    hkg.ghcid
                    hkg.hlint
                    less
                    sourceHighlight
                  ];

    shellHook = ''
      export NIX_PATH="nixpkgs=${nixpkgs}:."
    '';
  }

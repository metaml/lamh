{ nixpkgs ? import ./nix/nix.nix }:
let
  pkgs = import nixpkgs {};
  hkgs = pkgs.haskellPackages;
  ghc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    cabal-install
    zlib
  ]);

in
  with pkgs;

  mkShell {
    buildInputs = [
      binutils
      git
      gnumake
      gnumake
      hkgs.fswatcher
      hkgs.ghc
      hkgs.ghcid
      hkgs.hlint
      less
      sourceHighlight
    ];
  }

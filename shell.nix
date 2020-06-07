{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [];
  ghc = haskellPackages.ghcWithPackages haskellDeps;

  packages = [
    pkgs.ghc
    pkgs.binutils
    pkgs.gnumake
    pkgs.sourceHighlight
    pkgs.zlib
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.fswatcher
    haskellPackages.hlint
  ];
in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = packages;
  }

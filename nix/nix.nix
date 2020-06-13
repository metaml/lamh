# stolen from https://discourse.nixos.org/t/nix-haskell-development-2020/6170
let
  nix = import <nixpkgs> {};
  nixpin = nix.pkgs.lib.importJSON ./nix.json;

  nixpinned = nix.pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpin) rev sha256;
  };
in
  nixpinned

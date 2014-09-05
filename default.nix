let 
  nixpkgs = (import <nixpkgs> {}).fetchgit {
    url    = "git@github.com:nixos/nixpkgs.git";
    rev    = "718106e958cbd872ecf3e08a451b80f68f950dae";
    sha256 = "72ef1a4b66312676d0b7e1684d3d68a5e82fdff1725d8816a9dac7eff4ee81e8";
  };
in
{ system ? builtins.currentSystem
, pkgs ? (import nixpkgs { inherit system; })
, haskellPackages ? pkgs.haskellPackages_ghc783
, src ? ./.
, name ? "taggy"
}: 
haskellPackages.buildLocalCabal src name

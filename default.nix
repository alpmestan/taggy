let 
  nixpkgs = (import <nixpkgs> {}).fetchgit {
    url    = "git@github.com:zalora/nixpkgs.git";
    rev    = "f8bd9e70fab91607caa9e3039f4059db77540a54";
    sha256 = "3284674b4d7efd6290dce16837d1c67240aaae5f3d77b21e051f38faba6d7415";
  };
in
{ system ? builtins.currentSystem
, pkgs ? (import nixpkgs { inherit system; })
, haskellPackages ? pkgs.haskellPackages_ghc783
, src ? ./.
, name ? "taggy"
}: 
haskellPackages.buildLocalCabal src name

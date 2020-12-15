{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    ghc
    cabal-install
    haskellPackages.brittany
    haskellPackages.hlint
    haskellPackages.hoogle
    zlib
  ];
}

{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell { buildInputs = [ jdk leiningen clj-kondo ]; }

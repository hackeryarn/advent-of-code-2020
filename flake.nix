{
  description = "Development environment for chapter15";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        name = "chapter15";
        devShell = import ./shell.nix { inherit pkgs; };
      });
}

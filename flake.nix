{
  description = "mattyhall's website";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = {self, nixpkgs, flake-utils, flake-compat}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = (with pkgs; [
              bashInteractive
              cobalt
            ]);
          };
        }
    );
}

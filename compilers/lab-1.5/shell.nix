{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      gcc
      flex
      bison
      just

      bashInteractive
    ];
  }

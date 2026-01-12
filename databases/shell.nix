{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      dbgate

      bashInteractive
    ];
  }

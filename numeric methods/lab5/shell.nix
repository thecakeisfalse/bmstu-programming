{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      cargo
      clippy
      rustc
      rustfmt
      rust-analyzer

      bashInteractive
    ];
  }

{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      cargo
      rustc
      rust-analyzer
      rustfmt
      clippy

      bashInteractive
    ];
  }

{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      rustc
      rust-analyzer
      rustfmt
      cargo
      clippy

      bashInteractive
    ];
  }

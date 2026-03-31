{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      rustc
      cargo
      rustfmt
      rust-analyzer
      clippy

      bashInteractive
    ];
  }

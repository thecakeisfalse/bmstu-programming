{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      rustc
      rustfmt
      cargo
      rust-analyzer
      clippy

      bashInteractive
    ];
  }

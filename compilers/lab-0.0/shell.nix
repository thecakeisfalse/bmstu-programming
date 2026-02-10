{pkgs ? import <nixpkgs> {}}:
with pkgs;
mkShell {
    buildInputs = [
      cargo
      rustc
      rustfmt
      rust-analyzer
      clippy

      bashInteractive
    ];
  }

{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      rustc
      rustfmt
      rust-analyzer
      cargo

      clippy
      cargo-show-asm

      python313

      bashInteractive
    ];
  }

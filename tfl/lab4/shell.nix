{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      typst
      ghc
      cabal-install

      cargo
      rustc
      rust-analyzer
      rustfmt
      clippy

      pcre
      pkg-config

      bashInteractive
    ];
  }

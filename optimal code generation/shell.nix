{pkgs ? import <nixpkgs> {}}:
with pkgs; let
  llvm = llvmPackages_20;
in
  mkShell.override {
    inherit (llvm) stdenv;
  } {
    packages = [
      llvm.clang
      llvm.clang-tools
      llvm.libllvm

      pkg-config
      just

      gcc
      libgcc
      gmp

      rustc
      cargo
      rustfmt
      rust-analyzer
      clippy

      bashInteractive
    ];

    shellHook = ''
      export NIX_CFLAGS_COMPILE="-I$(gcc -print-file-name=plugin)/include $NIX_CFLAGS_COMPILE"
    '';
  }

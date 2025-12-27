{pkgs ? import <nixpkgs> {}}:
with pkgs; let
  llvm = llvmPackages_20;
  clangStdenv = llvm.stdenv;
in
  mkShell {
    nativeBuildInputs = [
      clangStdenv.cc
      clangStdenv.cc.bintools
    ];

    buildInputs = [
      llvm.libcxx
      llvm.compiler-rt

      bashInteractive
    ];

    LIBCXX_PATH = "${llvm.libcxx.dev}/include/c++/v1";
  }

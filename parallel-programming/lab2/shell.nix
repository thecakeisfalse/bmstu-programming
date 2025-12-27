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

      openmpi

      bashInteractive
    ];

    # LIBCLANG_PATH = ["${libclang.lib}/lib"];
    MPI_PATH = "${openmpi.dev}/include";
    LIBCXX_PATH = "${llvm.libcxx.dev}/include/c++/v1";
  }

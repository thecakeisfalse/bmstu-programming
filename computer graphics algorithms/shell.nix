{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      boost.dev
      glfw
      cmake
      gcc

      zig

      bashInteractive
    ];
  }

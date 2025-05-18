{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      zig

      libGL
      libGLU
      glfw
      glew

      boost.dev

      bashInteractive
    ];
  }

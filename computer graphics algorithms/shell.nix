{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      zig

      libGL
      libGLU
      freeglut
      glfw

      boost.dev

      bashInteractive
    ];
  }

{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell {
    buildInputs = [
      go
      gopls
      golangci-lint-langserver

      bashInteractive
    ];
  }

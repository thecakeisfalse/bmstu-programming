{pkgs ? import <nixpkgs> {}}:
with pkgs; let
  beam = beamMinimal28Packages;
in
  mkShell {
    buildInputs = [
      typst

      beam.erlang
      beam.erlfmt

      bashInteractive
    ];
  }

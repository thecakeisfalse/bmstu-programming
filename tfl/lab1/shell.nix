{pkgs ? import <nixpkgs> {}}:
with pkgs; let
  tex = texlive.combine {
    inherit
      (texlive)
      scheme-full
      latexmk
      tocvsec2
      ;
  };

  beam = beamMinimal28Packages;
in
  mkShell {
    buildInputs = [
      tex
      tk
      tcl

      beam.erlang
      beam.erlfmt

      bashInteractive
    ];
  }

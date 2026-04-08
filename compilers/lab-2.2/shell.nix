{pkgs ? import <nixpkgs> {}}:
with pkgs; let
  parser_edsl = {
    python3Packages,
    fetchFromGitHub,
    lib,
    ...
  }:
    python3Packages.buildPythonPackage {
      name = "parser_edsl";
      format = "other";

      src = fetchFromGitHub {
        owner = "bmstu-iu9";
        repo = "parser_edsl_python";
        rev = "2ee3737564710df32a177822c77e173e461f4ddd";
        hash = "sha256-TW7oIdkh1813UbGA1TMFT3rE+z1r9JBuraBp3+KeLn4=";
      };

      installPhase = ''
        mkdir -p $out/${python3Packages.python.sitePackages}
        cp parser_edsl.py $out/${python3Packages.python.sitePackages}/
      '';

      meta = {
        description = "A python library for writing compiler parsers";
        homepage = "https://github.com/bmstu-iu9/parser_edsl_python/";
        license = lib.licenses.mit;
      };
    };
  parser_edsl_pkg = callPackage parser_edsl {};
  python3 = python313.withPackages (ps: (with python313Packages; [
    parser_edsl_pkg
    jedi
    jedi-language-server
    python-lsp-server
    ruff
    ty
  ]));
in
  mkShell {
    buildInputs = [
      python3

      bashInteractive
    ];
  }

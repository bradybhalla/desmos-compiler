{
  description = "Dev shell for desmos-compiler (OCaml/dune)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ocamlPkgs = pkgs.ocamlPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            ocamlPkgs.ocaml
            ocamlPkgs.dune_3
            ocamlPkgs.findlib
            ocamlPkgs.core
            ocamlPkgs.core_unix
            ocamlPkgs.ppx_jane
            ocamlPkgs.odoc
            # Tooling
            ocamlPkgs.ocaml-lsp
            ocamlPkgs.ocamlformat
            ocamlPkgs.utop
          ];
        };
      });
}

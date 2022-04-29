{ pkgs ? import <nixpkgs> {} }:
  
with pkgs;
mkShell {
  nativeBuildInputs = (with ocaml-ng.ocamlPackages_4_13; [
    ocaml ocaml-lsp dune_2 menhir stdio merlin ocamlformat
    findlib
  ]) ++ [inotify-tools];
}
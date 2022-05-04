{ pkgs ? import <nixpkgs> {} }:
  
with pkgs;
mkShell {
  nativeBuildInputs = (with ocaml-ng.ocamlPackages_4_13; [
    ocaml stdio 
    bisect_ppx ppx_jane
    dune_2 menhir merlin ocamlformat findlib ocaml-lsp  
    alcotest
  ]) ++ [inotify-tools];

  shellHook = ''
    alias dune-watch="dune build -w"
    alias dune-test="dune runtest"
    alias dune-coverage="dune runtest --instrument-with bisect_ppx --force; bisect-ppx-report html; bisect-ppx-report summary"
    alias dune-run="dune exec gcic"
  '';
}
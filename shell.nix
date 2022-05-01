{ pkgs ? import <nixpkgs> {} }:
  
with pkgs;
mkShell {
  nativeBuildInputs = (with ocaml-ng.ocamlPackages_4_13; [
    ocaml stdio 
    ppx_expect bisect_ppx ppx_jane
    dune_2 menhir merlin ocamlformat findlib ocaml-lsp  
  ]) ++ [inotify-tools];
}
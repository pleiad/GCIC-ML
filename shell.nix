{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

with pkgs;
mkShell {
  nativeBuildInputs = (with ocamlPackages; [
    ocaml stdio fmt
    menhir menhirLib sedlex_2
    dune_2 merlin findlib ocaml-lsp utop
    alcotest qcheck qcheck-alcotest bisect_ppx
  ]) ++ [
    inotify-tools
    rlwrap
    ocamlformat
    ];

  shellHook = ''
    alias dune-watch="dune build -w"
    alias dune-test="dune runtest"
    alias dune-coverage="dune runtest --instrument-with bisect_ppx --force; bisect-ppx-report html; bisect-ppx-report summary"
    alias gcic-repl="sh scripts/repl.sh"
    alias update-parser-errors="sh lib/parsing/update_error_msgs.sh"
    alias dune-format="dune build @fmt --auto-promote"
  '';
}
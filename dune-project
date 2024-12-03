(lang dune 3.16)

(name func_prog_2)
(version 670fda0-dirty)

(generate_opam_files true)

(source
 (github fegor04/func-prog-2))

(authors "Egor Fedorov")

(maintainers "Egor Fedorov")

(license MIT)

(package
 (name func_prog_2)
 (synopsis "Binary Tree Dict")
 (description "Binary Tree Dict functor that allows you to create binary tree for any type")
 (depends ocaml dune (alcotest :with-test) qcheck qcheck-alcotest ppx_expect ocamlformat)
 (tags
  (btree dict)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

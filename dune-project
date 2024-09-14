(lang dune 3.16)

(name hello)

(generate_opam_files true)

(map_workspace_root false)

;(source
; (github username/reponame))

(authors "Samir Hadi")

(maintainers "Samir Hadi")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name hello)
 (synopsis "advent of code")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

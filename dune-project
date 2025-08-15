(lang dune 3.19)

(name pysem)

(generate_opam_files true)

(source
 (github Tchou/pysem))

(authors "Kim Nguyễn <kn@lmf.cnrs.fr>")

(maintainers "Kim Nguyễn <kn@lmf.cnrs.fr>")

(license LICENSE)

(documentation https://url/to/documentation)

(implicit_transitive_deps false)

(package
 (name pysem)
 (synopsis "A python type checker based on set-thoeoretic types (library)")
 (description "A python type checker based on set-thoeoretic types")
 (depends ocaml mlsem mlsem-lang sstt)
 (tags
  ("python" "static types" "type-checking" "org:universite-paris-saclay.fr")))

(package
 (name pysem-bin)
 (synopsis "A python type checker based on set-thoeoretic types (library)")
 (description "A python type checker based on set-thoeoretic types")
 (depends ocaml pysem)
 (tags
  ("python" "static types" "type-checking" "org:universite-paris-saclay.fr")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

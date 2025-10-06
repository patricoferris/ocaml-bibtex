(lang dune 3.19)
(using mdx 0.4)

(name bib)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-bibtex))

(authors "Patrick Ferris <patrick@sirref.org>")
(maintainers "Patrick Ferris <patrick@sirref.org>")

(license ISC)

(documentation https://url/to/documentation)

(package
 (name bib)
 (synopsis "OCaml Bibtex Parser")
 (description "A longer description")
 (depends
   ocaml
   bytesrw
   fmt)
 (tags
  ("bibtex")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

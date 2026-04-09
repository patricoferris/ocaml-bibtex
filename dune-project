(lang dune 3.19)
(using mdx 0.4)

(name bib)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-bibtex))

(authors "Patrick Ferris <patrick@sirref.org>")
(maintainers "Patrick Ferris <patrick@sirref.org>")

(license ISC)

(documentation https://ocaml.org/p/bib)

(package
 (name bib)
 (synopsis "OCaml Bibtex Codec")
 (description "A simple encoder and decoder for the Bibtex citation format.")
 (depends
   ocaml
   bytesrw
   astring
   fmt
   (mdx :with-test)
   (ocamlformat (and (= "0.28.1") :with-dev-setup))
 )
 (tags
  ("bibtex")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

(lang dune 3.10)

(name pretty_expressive)

(generate_opam_files true)

(source
 (github sorawee/pretty-expressive-ocaml))

(authors "Sorawee Porncharoenwase <sorawee.pwase@gmail.com>")

(maintainers "Sorawee Porncharoenwase <sorawee.pwase@gmail.com>")

(license ISC)

(documentation https://sorawee.github.io/pretty-expressive-ocaml/)

(version 0.5)

(using mdx 0.4)

(package
 (name pretty_expressive)
 (synopsis "A pretty expressive printer")
 (description "A pretty printer implementation of 'A Pretty Expressive Printer' (OOPSLA'23), with an emphasis on expressiveness and optimality.")
 (depends (ocaml (>= 4.05))
          dune
          (mdx      (and (>= 2.3.0) (<> 2.4.0) :with-test))
          (alcotest :with-test)))

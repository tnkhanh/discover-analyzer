(lang dune 3.0)


;; project name
(name discover)
(using menhir 2.1)

;; Run `dune build` to generate discover.opam
(generate_opam_files true)

;; Package
(package
 (name discover)
 (description "A source code static analyzer")
 (depends
  (dune (> 2.9))
  (core (>= 0.14.1))
  (ocamlgraph (>= 2.0.0))
  filetutils
  llvm
  yaml
  ezjsonm
  num))

;; Generate interface
(executables_implicit_empty_intf true)

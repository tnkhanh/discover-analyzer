(lang dune 2.9)

;; project name
(name discover)
(using menhir 2.1)

;; Run `dune build` to generate discover.opam
(generate_opam_files true)

;; Package
(package
 (name discover)
 (description "A source code static analyzer")
 (depends (core (>= 0.14.1))
          dune
          (ocamlgraph (>= 2.0.0))
          fileutils
          llvm
          yaml
          ezjsonm
          ; ocamlformat
          (ocamlformat :dev)  ;; use developing version of Ocamlformat
          ppx_deriving
          ppx_sexp_conv
          ppx_compare
          num))

;; Generate interface
; (executables_implicit_empty_intf true)        ;; must use dune >= 3.0

(lang dune 2.9)

(name discover)
(authors "Ta Quang Trung")
(maintainers "Ta Quang Trung")
(bug_reports "https://github.com/sbip-sg/discover/issues")
(homepage "https://github.com/sbip-sg/discover")
(using menhir 2.1)

;; Run `dune build` to generate the `discover.opam` file for this project
(generate_opam_files true)

;; Package
(package
 (name discover)
 (synopsis "A source code static analyzer")
 (description "A source code static analyzer.")
 (depends (core (>= 0.14.1))
          dune
          (ocamlgraph (>= 2.0.0))
          fileutils
          llvm
          yaml
          ezjsonm
          ; ocamlformat
          (ocamlformat :dev)  ;; use the developing version of Ocamlformat
          menhir
          num
          ppx_deriving
          ppx_sexp_conv
          ppx_compare))

;; Generate interface
; (executables_implicit_empty_intf true)        ;; must use dune >= 3.0

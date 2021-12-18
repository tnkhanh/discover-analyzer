(lang dune 2.9)

(name discover)
(authors "Ta Quang Trung")
(maintainers "Ta Quang Trung")
(bug_reports "https://github.com/sbip-sg/discover/issues")
(homepage "https://github.com/sbip-sg/discover")
(using menhir 2.1)


(generate_opam_files true)   ;; Run `dune build` to generate `discover.opam`

;; Package
(package
 (name discover)
 (synopsis "A source code static analyzer")
 (description "A source code static analyzer to find bugs in computer programs and smart contracts written in C/C++, Solidity, Golang, Typescript.")
 (depends (core (>= 0.14.1))
          dune
          (ocamlgraph (>= 2.0.0))
          fileutils
          ;; llvm             ;; LLVM bindings will be installed by llvm-sbip
          yaml
          ezjsonm
          (ocamlformat :dev)  ;; use the developing version of Ocamlformat
          (extcore :dev)
          menhir
          num
          ppx_deriving
          ppx_sexp_conv
          ppx_compare))

;; Generate interface
; (executables_implicit_empty_intf true)        ;; must use dune >= 3.0

Development Guide
======================

# IDE environment

- Suggested editor: Emacs, Vim, Visual Studio Code, Sublime Text, Atom, etc.

- Using [Ocp-indent](https://github.com/OCamlPro/ocp-indent) for automatic coding indentation, [merlin](https://github.com/ocaml/merlin) for code
  completion, navigation, outline, etc.

  ```sh
  opam install merlin ocp-indent
  ```

- Coding convention:
  + Use only white-spaces for indentation, each indentation is 2 whitespaces
  + The length of each code line is at most 80 column.
  + Naming style:
    * Functions and variables' names: snake case (`lower_case_with_underscore`):
    * Modules' names: Camel Case (`UpperCaseWithouthUnderscore`)

# Tutorials

- OCaml binding for LLVM:
  + https://llvm.org/docs/tutorial/OCamlLangImpl1.html
  + https://www.wzdftpd.net/blog/ocaml-llvm-01.html

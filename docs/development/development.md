Development Guide
======================

# IDE environment

- Suggested editor: Emacs, Vim, Visual Studio Code, Sublime Text, Atom, etc.

- Code completion, navigation, outline: use merlin
  ```sh
  opam install merlin ocp-indent
  ```

- Code indentation: 2 methods:
  + Use [ocp-indent](https://github.com/OCamlPro/ocp-indent) for automatic coding indentation: (very fast)

    ```sh
    opam install merlin ocp-indent
    ```

  + Use `ocamlformat`: general, can indent comments, (slower, but RECOMMENDED).

    ```sh
    opam install ocaml-lsp-server merlin ocamlformat ocamlformat-rpc
    ```

    Configuration is stored at `.ocamlformat` and `.ocamlformat-ignore` at the
    root of your project.

    To disable OCamlFormat for certain code, use `[@ocamlformat "disable"]`:

    ```ocaml
    let do_not_touch (x : t) (y : t) (z : t) = [
      x; y; z
    ] [@ocamlformat "disable"]
    ```

# Coding convention:

- Indentation should follow the rules of `ocp-indent`. See more settings in
  `discover/.ocp-indent`.

- Code format should follow `janestreet` style and settings in
  `discover/.ocamlformat`.

- Use only white-spaces for indentation, each indentation is 2 whitespaces

- The length of each code line is at most 80 column.

- Naming style:
  + Functions and variables' names: snake case (`lower_case_with_underscore`):
  + Modules' names: Camel Case (`UpperCaseWithouthUnderscore`)

- Use 2 blank lines between to separate functions.

- [Coding convention](https://opensource.janestreet.com/standards/) from Jane Street.

# Tutorials

- OCaml programming:
  + [Real World Ocaml](https://dev.realworldocaml.org/index.html): free, online book.

- OCaml binding for LLVM:
  + https://llvm.org/docs/tutorial/OCamlLangImpl1.html
  + https://www.wzdftpd.net/blog/ocaml-llvm-01.html

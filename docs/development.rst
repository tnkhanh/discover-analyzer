Development
==============

IDE and editors
-----------------

- Suggested editor: Emacs, Vim, Visual Studio Code.

- Code completion, navigation, outline by `merlin
  <https://github.com/ocaml/merlin>`_ and `ocaml-lsp
  <https://github.com/ocaml/ocaml-lsp>`_.

  .. code-block:: sh

     opam install merlin ocaml-lsp-server

- Code auto-formatting and indentation: `ocamlformat
  <https://github.com/ocaml-ppx/ocamlformat>`_ and `ocp-indent
  <https://github.com/OCamlPro/ocp-indent>`_:

  .. code-block:: sh

     opam install ocamlformat ocp-indent


Coding convention
-------------------

- Indentation by `ocp-indent <https://github.com/OCamlPro/ocp-indent>`_ and
  follow the rules configured in in ``discover/.ocp-indent``.

- Code auto-formatting by `ocamlformat
  <https://github.com/ocaml-ppx/ocamlformat>`_:

  + Use ``janestreet`` style and additional settings in
    ``discover/.ocamlformat`` and ``discover/.ocamlformat-ignore``.

  + To disable ocamlformat for certain code, use ``[@@ocamlformat "disable"]``:

    .. code-block:: ocaml

       let do_not_touch (x : t) (y : t) (z : t) = [
         x; y; z
       ] [@ocamlformat "disable"]

- Coding standards follow the book `Real World Ocaml
  <https://dev.realworldocaml.org/index.html>`_ (free, online).

- Every module must be accompanied by an ``*.mli`` interface file.

- Some general rules to write concise and readable code:

  + Use 2-white-space indentation style.

  + No trailing whitespaces at the end of every lines.

  + Each code line is at most 80 columns.

  + Function and variable names follow: ``snake_case_naming_style``.

  + Module and signature names follow: (``CamelCaseNamingStyle``).

  + Comment a region: always comment each line separately.

    .. code-block:: ocaml

       (* let list_head (lst: 'a list) : 'a option = *)
       (*   match lst with *)
       (*   | [] -> None *)
       (*   | hd :: tl -> Some tl  *)

Autoformat project using Dune
--------------------------------

- Require ``ocamlformat`` to be installed and the configuration file
  ``.ocamlformat`` at the project's root directory.

- Formatting the project by running ``dune``:

  .. code-block:: sh

     # Format the source code and display the differences
     dune build @fmt

     # Replace the source files by the corrected versions.
     dune promote

  or just run ``make format``:

  .. code-block:: sh

     make format

  and

  .. code-block:: ocaml

     let x = 1 in
     let b = 2 in
     let x = 1 in


- Read more at this `formatting project tutorial
  <https://dune.readthedocs.io/en/stable/formatting.html>.`_

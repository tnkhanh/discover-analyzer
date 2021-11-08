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
-------------------------------

- Indentation by `ocp-indent <https://github.com/OCamlPro/ocp-indent>`_ and
  follow the rules configured in in ``discover/.ocp-indent``.

- Code auto-formatting by `ocamlformat
  <https://github.com/ocaml-ppx/ocamlformat>`_:

  + Use ``janestreet`` style and additional settings in
    ``discover/.ocamlformat`` and ``discover/.ocamlformat-ignore``.

  + To disable ocamlformat for certain code, use ``[@ocamlformat "disable"]``:

    .. code-block:: ocaml

       let do_not_touch (x : t) (y : t) (z : t) = [
         x; y; z
       ] [@ocamlformat "disable"]

- Coding standards follow the book `Real World Ocaml
  <https://dev.realworldocaml.org/index.html>`_ (free, online).

- Some general rules to write concise and readable code:

  + Use 2-white-space indentation style.
  + Each code line is at most 80 column.
  + Function and variable names follow snake case style (``snake_case_naming``).
  + Module and signature names follow Camel Case (``CamelCaseNaming``).

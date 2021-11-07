Development
==============

IDE and editors
-----------------

- Suggested editor: Emacs, Vim, Visual Studio Code.

- Code completion, navigation, outline by ``merlin`` and ``ocamllsp``.

  .. code-block:: sh

     opam install merlin ocp-indent

- Code indentation: ``ocp-indent`` and ``ocamlformat``:

  .. code-block:: sh

     opam install merlin ocp-indent


Coding format and convention
-------------------------------

- Indentation by ``ocp-indent`` and follow the rules in
  ``discover/.ocp-indent``.

- Code auto-formatting by ``ocamlformat``

  + Use ``janestreet`` style and settings in ``discover/.ocamlformat`` and
    ``discover/.ocamlformat-ignore``.

  + To disable ocamlformat for certain code, use ``[@ocamlformat "disable"]``:

    .. code-block:: ocaml

       let do_not_touch (x : t) (y : t) (z : t) = [
         x; y; z
       ] [@ocamlformat "disable"]

- Other general rules:

  + Use 2-white-space indentation style.
  + Each code line is at most 80 column.
  + Function and variable names follow snake case style (``snake_case_naming``).
  + Module and signature names follow Camel Case (``CamelCaseNaming``).

- More coding standards
  [Coding convention]() from Jane Street.

OCaml tutorials
-----------------

- OCaml programming:

  + `Real World Ocaml <https://dev.realworldocaml.org/index.html>`_: free, online book.

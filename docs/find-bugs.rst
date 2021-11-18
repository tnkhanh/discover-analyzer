Use Discover to find bugs
==================================

Integer bugs
^^^^^^^^^^^^^^^

- Find integer bugs in C files using range analysis:

  .. code-block:: sh

     discover --clang-option "-I lib/discover/ -g" \
              --dfa-inter --dfa-range --bug-integer-all \
              input-file.c

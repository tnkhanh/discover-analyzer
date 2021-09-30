Brainstorming on the design of Discover
-------------------------------------------

# Goal

- Automatic verification and testing.
- Do not require on users to manually provide specification.

# Analysis techniques

- Using two techniques: abstract interpretation and horn clause verification.

- Abstract interpretation:
  + Used for big programs, large scale projects.
  + Fast, but maybe less precise.

- Horn clause verification is used for small programs.
  + Use Separation Logic as the underlying theory
  + Used for small programs, small scale projects.
  + Slow, but highly precise.

# Combining multiple analyses

- Using only one analysis might not be enough for detecting bugs. For example,
  to detect a buffer-overflow we might need to use 3 analyses:

  + Pointer analysis: to capture aliasing information of pointers.
  + Memory size analysis: to capture allocated size of buffers.
  + Range analysis: to capture value of accessing indices.

# Specifying bugs:

- Specifying bugs using annotations:

  ```c
  #include <stdio.h>

  void main(int argc, char** argv) {
    int a = 1;
    printf("Input an integer: ");
    scanf("%d", &a);

    // Potential integer overflow at `a *  4`.
    int x = /*{Bug:IntegerOverflow*/ a * 4 /*:Bug}*/;

    // Potential integer overflow at `a * 10`.
    long y = /*{Bug:IntegerOverflow*/ a * 10 /*:Bug}*/;

    // Potential integer overflow at `a * 10`.
    long b = a;
    long y = /*{Safe:IntegerOverflow*/ b * 10 /*:Safe}*/;

  }
  ```

- Discover will analyze the program to find bugs, and read the annotations to
  check if a bug really happens as annotated.

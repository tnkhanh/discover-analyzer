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

# Specifying bugs:

- Two methods: using annotation (comments) or assertions (code instructions)?

- Sample program:

  ```c
  #include <stdio.h>

  void main(int argc, char** argv) {
    int a = 1;
    printf("Input an integer: ");
    scanf("%d", &a);

    int x = a * 4;       // Potential integer overflow at `a * 4`.

    long y = a * 10;     // Potential integer overflow at `a * 10`.
  }
  ```

- Specifying bugs using annotations:

  ```c
  #include <stdio.h>

  void main(int argc, char** argv) {
    int a = 1;
    printf("Input an integer: ");
    scanf("%d", &a);

    // Potential integer overflow at `a *  4`.
    int x = /*{bug:integer_overflow*/ a * 4 /*:bug}*/;

    // Potential integer overflow at `a * 10`.
    long y = /*{bug:integer_overflow*/ a * 10 /*:bug}*/;
  }
  ```

- Specifying bugs using assertions:

  ```c
  #include <stdio.h>

  void main(int argc, char** argv) {
    int a = 1;
    printf("Input an integer: ");
    scanf("%d", &a);

    // Potential integer overflow at `a *  4`.
    int x = a * 4;
    __assert_integer_overflow(&x, sizeof(x));  // need type infor of x

    // Potential integer overflow at `a * 10`.
    long y = a * 10;
    __assert_integer_overflow(&y, sizeof(y));  // need type infor of y
  }
  ```

- Points need to consider when specifying bugs:
  + Bug location needs to be as precise as possible.
  + Metadata information needs to be maintained during the compilation, and is
    accessible at the LLVM IR level.
  + What else?

- QUESTION: which method is better to specify bugs?

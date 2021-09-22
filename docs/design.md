Design choice
--------------

# Goal

- Automatic verification and testing.
- Do not require on users to manually provide specification.

# Design choice of Discover:

- Using two techniques: abstract interpretation and horn clause verification.

- Abstract interpretation:
  + Used for big programs, large scale projects.
  + Fast, but maybe less precise.

- Horn clause verification is used for small programs.
  + Used for small programs, small scale projects.
  + Slow, but highly precise.

# Verification using Horn clause:

- Use separation logic theory.

- Challenges:
  + How to handle array?

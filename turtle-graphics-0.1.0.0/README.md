Chalmers Advanced Functional Programming Assignment #1

- The design starts by defining single turtle program as transforming one turtle state to another. This is closer to a shallow implementation.
- Starting from a different branch, defining a general multi-state program with the state trace logged (Writer).
  - The serial combinator follows naturally from monadic bind.
  - The parallel combinator needs to aggregate the result states (and traces) of both child programs.
  - It's then obvious the graphics is a log (Writer) of the trace.
- To combine the two design branches, single turtle programs are lifted to the multiple state version.
- The graphics is done with OpenGL.
# Advent of Code 

Here are my Advent of Code solutions written in Common Lisp.

To load and run:

```lisp
(ql:quickload :aoc)
(in-package :aoc)
```

To update the system definition after solving a problem:
- add a `:component` to `aoc.asd`
- add an `:export` for the added functions in `package.lisp`
- run `(asdf:clear-configuration)` and `(asdf:make :aoc)`

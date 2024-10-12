# r7rs [![build](https://github.com/lexi-lambda/racket-r7rs/actions/workflows/build.yml/badge.svg)](https://github.com/lexi-lambda/racket-r7rs/actions/workflows/build.yml)

This is an implementation of [R7RS "small"][r7rs] in [Racket][racket]. To use it, install the package and use `#lang r7rs` in your programs.

```sh
$ raco pkg install r7rs
```

```scheme
#lang r7rs

(import (scheme base)
        (scheme write))

(display (string-append "Hello, " "world!"))
```

## Current Support

All of the libraries defined in R7RS are available. However, there are a few minor missing features or inconsistencies:

  - `(scheme base)` — Implemented except for `include-ci`. Furthermore, the `define-library` form is restricted to be more compatible with the Racket module system.
  - `(scheme process-context)` — All bindings are available, but `exit` and `emergency-exit` are the same, and `exit` does not properly call outgoing `dynamic-wind` thunks.

All other libraries are fully supported.

[racket]: http://racket-lang.org
[r7rs]: https://small.r7rs.org/

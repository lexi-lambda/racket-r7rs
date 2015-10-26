# r7rs [![Build Status](https://travis-ci.org/lexi-lambda/racket-r7rs.svg?branch=master)](https://travis-ci.org/lexi-lambda/racket-r7rs)

This is an **incomplete, work-in-progress** implementation of [R7RS "small"][r7rs] in [Racket][racket]. To use it, install the package and use `#lang r7rs` in your programs.

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

All of the libraries defined in R7RS are available. However, a few of them are incomplete or slightly incorrect:

  - `(scheme base)` — Mostly implemented, but some of the more complicated forms like `include-ci` aren't in yet. Furthermore, creating R7RS libraries via the `define-library` form is *not* supported, though similar nonstandard functionality is possible since `import` cooperates with the Racket module system.
  - `(scheme process-context)` — All bindings are available, but `exit` and `emergency-exit` are the same, and `exit` does not properly call outgoing `dynamic-wind` thunks.
  - `(scheme read)` — Implemented, but uses the R5RS reader as described below.
  - `(scheme write)` — Uses the R5RS printer as described below. Also, `write-shared` and `write-simple` both operate in the same way as `write`.

All other libraries are fully supported.

Additionally, the current implementation uses the regular R5RS reader and printer, so all R7RS incompatibilities with the R5RS reader are not yet supported.

[racket]: http://racket-lang.org
[r7rs]: http://trac.sacrideo.us/wg/wiki/R7RSHomePage

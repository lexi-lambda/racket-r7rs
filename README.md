# r7rs [![Build Status](https://travis-ci.org/lexi-lambda/racket-r7rs.svg?branch=master)](https://travis-ci.org/lexi-lambda/racket-r7rs)

This is an **incomplete, work-in-progress** implementation of [R7RS "small"][r7rs] in [Racket][racket]. To use it, install the package and use `#lang r7rs` in your programs.

```sh
$ raco pkg install r7rs
```

```scheme
#lang r7rs

(import (scheme base))

(display (string-append "Hello, " "world!"))
```

## Current Support

The following parts of R7RS are currently supported:

  - The following libraries are *fully* supported:
    - `(scheme case-lambda)`
    - `(scheme char)`
    - `(scheme complex)`
    - `(scheme cxr)`
    - `(scheme eval)`
    - `(scheme file)`
    - `(scheme inexact)`
    - `(scheme lazy)`
    - `(scheme load)`
    - `(scheme repl)`
  - The following libraries are supported *with caveats*:
    - `(scheme base)` — Mostly implemented, but some of the more complicated forms like `include-ci` aren't in yet.
    - `(scheme process-context)` — All bindings are available, but `exit` and `emergency-exit` are the same, and `exit` does not properly call outgoing `dynamic-wind` thunks.
    - `(scheme read)` — Implemented, but uses the R5RS reader as described below.

No other libraries are supported at this time. Furthermore, creating R7RS libraries via the `define-library` form is *not* supported, though similar nonstandard functionality is possible since `import` cooperates with the Racket module system. Finally, the current implementation uses the regular R5RS reader, so all R7RS incompatibilities with the R5RS reader are not yet supported.

[racket]: http://racket-lang.org
[r7rs]: http://trac.sacrideo.us/wg/wiki/R7RSHomePage

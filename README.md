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

  - *Most* of `(scheme base)` is supported. Some of the more complicated forms like `include-ci` aren't in yet.
  - *All* of `(scheme case-lambda)` is supported.
  - *All* of `(scheme char)` is supported.

No other libraries are supported at this time. Furthermore, creating R7RS libraries via the `define-library` form is *not* supported, though similar nonstandard functionality is possible since `import` cooperates with the Racket module system. Finally, the current implementation uses the regular R5RS reader, so all R7RS incompatibilities with the R5RS reader are not yet supported.

[racket]: http://racket-lang.org
[r7rs]: http://trac.sacrideo.us/wg/wiki/R7RSHomePage

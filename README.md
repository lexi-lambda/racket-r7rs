# r7rs [![Build Status](https://travis-ci.org/lexi-lambda/racket-r7rs.svg?branch=master)](https://travis-ci.org/lexi-lambda/racket-r7rs)

This is an **incomplete, work-in-progress** implementation of [R7RS "small"][r7rs] in [Racket][racket]. To use it, install the package and use `#lang r7rs` in your programs.

```scheme
#lang r7rs

(import (scheme base))

(display (string-append "Hello, " "world!"))
```

## Current Support

The following parts of R7RS are currently supported:

  - *All* of `(scheme base)` is supported.
  - *All* of `(scheme case-lambda)` is supported.
  - *All* of `(scheme char)` is supported.

No other libraries are supported at this time. Additionally, the current implementation uses the regular R5RS reader, so all R7RS incompatibilities with the R5RS reader are not yet supported.

[racket]: http://racket-lang.org
[r7rs]: http://trac.sacrideo.us/wg/wiki/R7RSHomePage

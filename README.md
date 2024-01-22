# Effect Racket

[![Build Status][build-badge]][build]
[![Scribble][docs-badge]][docs]

## Install

```
$ raco pkg install effect-racket
```

## Organization

This repository is organized into several separate packages.

* `effect-racket-doc/`
  contains the documentation for how to use the library.
* `effect-racket-lib/`
  contains the main code for the `effect-racket` language
  and library.
* `effect-racket-test/`
  contains both examples from the paper
  and well as the unit and integration tests
  for the implementation.
* `scripts/`
  has miscellaneous files that, for example, build
  the artifact Docker image.

[build-badge]: https://github.com/camoy/effect-racket/actions/workflows/build.yml/badge.svg
[build]: https://github.com/camoy/effect-racket/actions/workflows/build.yml?query=workflow%3Abuild
[docs-badge]: https://img.shields.io/badge/Docs-Scribble-blue.svg
[docs]: https://docs.racket-lang.org/effect-racket

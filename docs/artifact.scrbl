#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[racket/base
                    racket/contract
		    racket/match
                    effect-racket]]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; URLS

@(define RKT-DOWNLOADS "https://download.racket-lang.org/all-versions.html")
@(define EFFECT-DOCS '(lib "effect-racket-doc/scribblings/effect-racket.scrbl"))
@(define GUIX "https://guix.gnu.org/")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Artifact: Effectful Software Contracts}
@author{Cameron Moy}
@author{Christos Dimoulas}
@author{Matthias Felleisen}

This artifact accompanies the paper "Effectful Software Contracts." Evaluating
the artifact will require you to read, understand, and execute sample code
from the paper. There is no experimental evaluation to perform.

@section{Phase I: Install and Test}

You can either run the artifact in a pre-built Docker image that includes all
of the necessary dependencies or manually install the artifact.

@subsection{Docker}

The Docker image is based on @hyperlink[GUIX]{Guix System} and contains only
a very limited number of tools: bash, coreutils, nano, and vim. Perform a
manual installation if you want to use your own development environment.

@itemlist[
  @item{
    Download, import, and run the Docker image:
    @verbatim{
$ wget ccs.neu.edu/~camoy/artifact/effect-racket-artifact.tar.gz
$ docker load --input effect-racket-artifact.tar.gz
$ docker run -ti effect-racket-artifact
$ cd effect-racket
    }
  }

  @item{
    All of the package's tests should pass:
    @verbatim{
$ raco test effect-racket-test
    }
  }
]

@subsection{Manual}

@itemlist[
  @item{
    Install @hyperlink[RKT-DOWNLOADS]{Racket 8.10}.
  }

  @item{
    Run the following commands to download and install the artifact:
    @verbatim{
$ git clone https://github.com/camoy/effect-racket
$ cd effect-racket
$ # Don't omit the trailing slashes!
$ raco pkg install --auto effect-racket-test/ effect-racket-lib/
    }
  }

  @item{
    All of the package's tests should pass:
    @verbatim{
$ raco test effect-racket-test
    }
  }
]

@section{Phase II: Examples}

@itemlist[
  @item{
    Please read (at least) Sections 1, 2, 5, and 7.1 of the submitted paper.
  }

  @item{
    Section 7.1 describes a series of minimal examples that cover the kinds
    of contracts other systems can express. These examples are included in
    the @exec{effect-racket-test/examples} directory. Take a look at some (or all)
    of these examples and make sure they make sense and correspond to the
    descriptions included in the paper.
  }

  @item{
    See the @hyperlink["effect-racket.html"]{Effect Racket documentation} and
    check that it is sufficient to use the library. If you feel comfortable,
    try out the library on your own examples!
  }
]

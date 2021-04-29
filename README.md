# linear-algebra

## Goal
I have implemented some matrix algebra.

## How to run
Install Common Lisp. I am using `sbcl`.  You will need `quicklisp` so install this too. Finally, when you have installed the above you need to load the test suite `fiasco`(ie `(ql:quickload :fiasco)`) and load the file ie `(load "linear-algebra.lisp")`.

Once you have done this, to run the tests you do: `(in-package :linear-algebra-tests)` and then `(run-package-tests)`.


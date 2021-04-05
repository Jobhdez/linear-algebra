# linear-algebra

My goal is to implement a lot of linear algebra and see If ends up in a library. Another possibility is to write a simple interpreter and allow users to express their solutions using a linear algebra type mini language.

## How to run
Install Common Lisp. I am using `sbcl`.  You will need `quicklisp` so install this too. Finally, when you have installed the above you need to load the test suite `fiasco`(ie `(ql:quickload :fiasco)`) and load the file ie (load "linear-algebra.lisp").

Once you have done this, to run the tests you do: `(in-package :linear-algebra-tests)` and then `(run-package-tests)`.


;;;; tests/suite.lisp
;;;;
;;;; Author: Joseph Lin(@ github.com/rigetti/magicl)

(in-package #:linear-algebra-tests)

(defun run-linear-algebra-tests (&key (verbose nil) (headless nil))
  "Run all DEEPMATH tests"
  (setf fiasco::*tests-run-standard-output* (make-broadcast-stream
					     *standard-output*))
  (cond
   ((null headless)
    (run-package-tests :package ':linear-algebra-tests
		       :verbose verbose
		       :describe-failures t
		       :interactive t))
   (t
    (let ((successp (run-package-tests :package ':linear-algebra-tests
				       :verbose t
				       :describe-failures t
				       :interactive nil)))
      (uiop:quit (if successp 0 1))))))

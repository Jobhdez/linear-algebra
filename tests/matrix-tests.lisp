;;;; tests/matrix-tests.lisp
;;;;
;;;; Author: Job Hernandez

(in-package #:linear-algebra-tests)

(deftest test-determinant ()
  "Test if DETERMINANT works."
  (is (equal (determinant '((1 -5 2) (7 3 4) (2 1 5)) 1)
	     148)))

(deftest test-minor ()
  "Test if MINOR works."
  (is (equal (minor '((2 3 4) (5 6 7) (8 9 10)) 2 1)
	     '((3 4) (9 10)))))

(deftest test-cofactor ()
  "Test if COFACTOR works."
  (is (equal (cofactor '((2 3 4) (5 6 7) (8 9 10)) 2 1) 6)))

(deftest test-tref ()
  "Test if TREF works."
  (is (equal (tref '((3 4) (4 5) (6 7)) 3 2) 7)))

(deftest test-trace ()
  "Test if COMPUTE-TRACE works."
  (is (equal (compute-trace '((2 3 4) (6 8 9) (1 10 12))) 22)))

(deftest test-upper-triangular ()
  "Test if UPPER-TRIANGULAR works."
  (is (equal (upper-triangular '((2 3 4) (5 6 7) (8 9 10)))
	     '((2 3 4) (0 6 7) (0 0 10)))))

(deftest test-2-upper-triangular ()
  "Test if UPPER-TRIANGULAR works on a 4 by 3 matrix."
  (is (equal (upper-triangular '((2 3 4) (5 6 7) (8 9 10) (11 12 12)))
	     '((2 3 4) (0 6 7) (0 0 10) (0 0 0)))))

(deftest test-3-upper-triangular ()
  "Test if UPPER-TRIANGULAR  works on a 3 by 4 matrix."
  (is (equal (upper-triangular '((3 4 5 6) (7 8 9 10) (11 12 13 14)))
	     '((3 4 5 6) (0 8 9 10) (0 0 13 14)))))

(deftest test-lower-triangular ()
  "Test if LOWER-TRIANGULAR works on a square matrix."
  (is (equal (lower-triangular '((2 3 4) (5 6 7) (8 9 10)))
	     '((2 0 0) (5 6 0) (8 9 10)))))

(deftest test-2-lower-triangular ()
  "Test if LOWER-TRIANGULAR works on a 4 by 3 matrix."
  (is (equal (lower-triangular '((2 3 4) (5 6 7) (8 9 10) (11 12 13)))
	     '((2 0 0) (5 6 0) (8 9 10) (11 12 13)))))

(deftest test-3-lower-triangular ()
  "Test if LOWER-TRIANGULAR works on a 3 by 4 matrix."
  (is (equal (lower-triangular '((2 3 4 5) (6 7 8 9) (10 11 12 13)))
	     '((2 0 0 0) (6 7 0 0) (10 11 12 0)))))

(deftest test-transpose ()
  "Test if TRANSPOSE works."
  (is (equal (transpose '((2 3 4) (5 6 7))) '((2 5) (3 6) (4 7)))))

(deftest test-make-matrix-of-minors ()
  "Test if MAKE-MATRIX-OF-MINORS works."
  (is (equal (make-matrix-of-minors '((-1 -2 2) (2 1 1) (3 4 5)) 1 1)
	     '((1 -7 5) (18 -11 -2) (-4 5 3)))))

(deftest test-inverse ()
  "Test if INVERSE works."
  (is (equal (inverse '((-1 -2 2) (2 1 1) (3 4 5)))
	     '((1/23 18/23 -4/23) (-7/23 -11/23 5/23) (5/23 -2/23 3/23)))))

(deftest test-add-matrices ()
  "Test if ADD-MATRICES work."
  (is (equal (get-matrix (add-matrices (make-matrice! 2 3 '((1 2 3) (4 5 6)))
				       (make-matrice! 2 3 '((1 2 3) (4 5 6)))))
	     '((2 4 6) (8 10 12)))))

(deftest test-mul-matrices ()
  "Test if MULTIPLY-MATRICES work."
  (is (equal (get-matrix (multiply-matrices (make-matrice! 2 3 '((1 2 3) (4 5 6)))
					     (make-matrice! 2 3 '((7 8) (9 10) (11 12)))))
	     '((58 64) (139 154)))))

(deftest test-matrix-by-scalar ()
  "Test if MULTIPLY-BY-SCALAR works."
  (is (equal (multiply-by-scalar 5 '((2 3 4) (5 6 7)))
	     '((10 15 20) (25 30 35)))))

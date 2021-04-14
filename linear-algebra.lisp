;;; Author: Job Hernandez
;;; linear-algebra.lisp

(defpackage #:linear-algebra
  (:use #:common-lisp)
  (:export #:determinant
	   #:minor
	   #:cofactor
	   #:compute-trace
	   #:tref
	   #:upper-triangular
	   #:lower-triangular
	   #:transpose))

(in-package #:linear-algebra)


;; the Determinant whose elements are aij is denoted
;; |aij|

;; the Minor Mij is the remaining Determinant obtained
;; when removing the column and row containg the element
;; aij

;; the Cofactor of aij is the Signed Minor (-1)^i+j*Mij

(defun determinant (matrix row)
  "Computes the determinant of a given matrix using 
  the specified row."
  ;; Matrix -> Number
  ;; given: '((1 -5 2) (7 3 4) (2 1 5)) 1
  ;; expect:  148
  (let ((row1 (getrow matrix row)))
    (defun determinant! (givenrow matrix column)
      (if (null givenrow)
	  0
	(+ (* (car givenrow)
	      (cofactor matrix row column))
	   (determinant! (cdr givenrow)
			 matrix
			 (+ column 1)))))
    (determinant! row1 matrix 1)))

(defun compute-trace (matrix)
  "Computes the sum of the diagonal elements(ie trace) of the matrix."
  ;; Matrix -> Sum
  ;; given: (trace '((2 3 4) (6 8 9) (1 10 12)))
  ;; expect: 22
  (loop for i from 1 to (length (car matrix))
	sum (tref matrix i i)))

(defun upper-triangular (matrix)
  "Computes the upper triangular of a given matrix."
  ;; Matrix -> Matrix
  ;; given: (upper-triangular '((2 3 4) (5 6 7) (8 9 10)))
  ;; expect: '((2 3 4) (0 6 7) (0 0 10))
  (defun upper! (matrix i)
    "Helps UPPER-TRIANGULAR -- it computes the upper-triangular."
    (if (null matrix)
	'()
      (cons (fill-with-zeros (car matrix) i)
	    (upper! (cdr matrix) (1+ i)))))
  
  (defun fill-with-zeros (vec n)
    "Replaces the n elements in vec with n zeros."
    (cond ((= n 0) vec)
	  ((= n 1) (cons 0 (cdr vec)))
	  (t (cons 0
		   (fill-with-zeros (cdr vec) (1- n))))))
  
  (upper! matrix 0))
  
(defun lower-triangular (matrix)
  "Computes the lower-triangular of a given matrix."
  ;; Matrix -> Matrix
  ;; given: (lower-triangular '((2 3 4) (5 6 7) (8 9 10)))
  ;; expect: '((2 0 0) (5 6 0) (8 9 10))
  (defun lower! (matrix i)
    "Helps LOWER-TRIANGULAR -- it computes lower-triangular."
    (if (null matrix)
	'()
      (cons (reverse (fill-with-zeros
		      (reverse (car matrix))
		      i))
	    (lower! (cdr matrix) (1- i)))))
  
  (defun fill-with-zeros (vec n)
    "Replaces n elements in vec with n zeros."
    (cond ((= n -1) vec)
	  ((= n 0) vec)
	  (t (cons 0
		   (fill-with-zeros (cdr vec) (1- n))))))
  
  (lower! matrix (- (length (car matrix)) 1)))

(defun transpose (matrix)
  "Computes the transpose of the matrix."
  ;; Matrix -> Matrix
  ;; given: (transpose '((2 3 4) (5 6 7)))
  ;; expect: '((2 5) (3 6) (4 7))
  (defun getfirst (matrix)
    (mapcar #'car matrix))
  (defun getcdr (matrix)
    (mapcar #'cdr matrix))
  (defun null-matrixp (matrix)
    (if (null matrix)
	T
      (and (null (car matrix))
	   (null-matrixp (cdr matrix)))))
  (if (null-matrixp matrix)
      '()
    (cons (getfirst matrix)
	  (transpose (getcdr matrix)))))

(defun cofactor (matrix row column)
  "Computes the cofactor from the minor of the matrix at Aij."
  ;; Matrix Row Counter
  ;; given: '((2 3 4) (5 6 7) (8 9 10)) 2 1
  ;; expect: 6
  (defun determinant2d (matrix)
    (- (* (first (first matrix)) (second (second matrix)))
       (* (second (first matrix)) (first (second matrix)))))
  (* (expt -1 (+ row column))
     (determinant2d (minor matrix row column))))

(defun minor (matrix row column)
  "Computes the minor of a matrix."
  ;; Matrix Row Column -> Matrix
  ;; given: '((2 3 4) (5 6 7) (8 9 10)) 2 1
  ;; expect: '((3 4) (9 10))
  (defun minor! (matrix col)
    "Takes a matrix with all rows except one, specified by 'row',
    and finishes computing the minor."
    (if (null matrix)
	'()
      (cons (removecol (car matrix) col)
	    (minor! (cdr matrix)
		    col))))
  (minor! (removerow matrix row)
	  column))

(defun tref (matrix i j)
  "Gets the element in row i and column j in the matrix."
  ;; Matrix Number Number -> Number
  ;; given: (tref '((3 4) (4 5) (6 7)) 3 2)
  ;; 7
  (let ((row (getrow matrix i)))
    (getcolumn row j)))

(defun getrow (matrix row)
  "Gets the row, specified by 'row', from the a given matrix."
  ;; Matrix row -> row
  ;; given: (getrow '((2 3) (8 9) (3 4)) 2)
  ;; expect: '(8 9)
  (defun getrowiter (matrix row counter)
    (if (= counter row)
	(car matrix)
      (getrowiter (cdr matrix) row (+ counter 1))))
  (getrowiter matrix row 1))

(defun getcolumn (vector column)
  "Gets the column from vector."
  (defun getcolumniter (vector column counter)
    (if (= counter column)
	(car vector)
      (getcolumniter (cdr vector) column (+ counter 1))))
  (getcolumniter vector column 1))
    
(defun removerow (matrix row)
  "Removes the given row from the matrix."
  (removerowiter matrix row 1))

(defun removerowiter (matrix row counter)
  "Iterator for REMOVEROW."
  (if (= counter row)
      (cdr matrix)
    (cons (car matrix)
	  (removerowiter (cdr matrix)
			 row
			 (+ 1 counter)))))

(defun removecol (vec col)
  "Removes the column from a vector."
  (removecoliter vec col 1))

(defun removecoliter (vec col counter)
  "Iterator for REMOVECOL."
  (if (= counter col)
      (cdr vec)
    (cons (car vec)
	  (removecoliter (cdr vec)
			 col
			 (+ counter 1)))))
  
	     		   
(fiasco:define-test-package #:linear-algebra-tests
			    (:use #:linear-algebra))
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

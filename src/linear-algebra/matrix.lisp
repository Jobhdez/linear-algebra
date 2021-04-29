;;; Author: Job Hernandez
;;; linear-algebra.lisp


(in-package #:linear-algebra)

(defstruct matrix
  "A Matrix is a structure: (make-matrix row column contents)."
  rows
  columns
  contents)

(defun get-rows (m)
  "Given a Matrix structure it gets the number of rows."
  ;; Matrix -> Number
  (matrix-rows m))

(defun get-columns (m)
  "Given a Matrix structure it returns the number of columns."
  ;; Matrix -> Number
  (matrix-columns m))

(defun get-matrix (m)
  "Given a Matrix structure it returns a List representing the contents of the structure."
  ;; Matrix -> List
  (matrix-contents m))

(defun make-matrice! (rows columns contents)
  "It takes the number of rows, and number of columns, and the contents, and it 
   returns a Matrix structure."
  ;; Number Number List -> Structure
  (make-matrix :rows rows
	       :columns columns
	       :contents contents))

(defun add-matrices (m m2)
  "Takes two Matrix structures and returns a Matrix structure whose contents represent 
   the addition of the contents of m and m2."
  ;; Structure Structure -> Structure
  (let ((m* (get-matrix m))
	(m2* (get-matrix m2)))
    (make-matrice! (get-rows m)
		   (get-columns m2)
		   (compute m* #'+ m2*))))

(defun subtract-matrices (m m2)
  "Takes two Matrix structures and returns a Matrix structure whose contents represent 
   the subtraction of the contents of the matrix structures m and m2."
  ;; Structure Structure -> Structure
  (let ((m* (get-matrix m))
	(m2* (get-matrix m2)))
    (make-matrice! (get-rows m)
		   (get-columns m)
		   (compute m* #'- m2*))))

(defun compute (m op m2)
  "Takes the contents of two Matrix structures and applies computation to them."
  ;; List Symbol List -> List
  (loop for i in m
	for j in m2
	collect (compute! op i j)))

(defun compute! (op v v2)
  "Takes an operator and applies it to two vectors."
  ;; Symbol List List -> List
  (loop for i in v
	for j in v2
	collect (funcall op i j)))

(defun multiply-by-scalar (scalar matrix)
  "Multiplies a scalar by a matrix."
  (defun row-by-scalar (row)
    (loop
     for i in row
     collect (* i scalar)))
  (loop for i in matrix
        collect (row-by-scalar i)))

(defun multiply-matrices (m m2)
  "Multiplies two matrices."
  ;; Structure Structure -> Structure
  (let ((m* (get-matrix m))
	(m2* (get-matrix m2)))
    (make-matrice! 2 2 (multiply-ms m* m2*))))

(defun multiply-ms (m m2)
  "Does the actual multiplication(of the contents of two Matrix structures) of 
   MULTIPLY-MATRICES."
  ;; List List -> List
  (if (null m)
     '()
    (cons (compute-entry m m2)
	  (multiply-ms (cdr m) m2))))

(defun compute-entry (m m2)
  "Computes an entry of a new matrix."
  ;; List List -> List
  (if (nullmatrix m2)
      '()
    (cons (add-entries (multiply-vectors (first-row m)
					 (first-column m2)))
	  (compute-entry m (getcdr m2)))))

(defun multiply-vectors (v v2)
  "Multiplies two vectors."
  (loop for i in v
	for j in v2
	collect (* i j)))

(defun nullmatrix (m)
  "Checks if the matrix is null."
  ;; List -> Bool
  (if (null m)
      T
    (and (null (car m))
	 (nullmatrix (cdr m)))))
	       
(defun first-row (m)
  "Gets the first row of the matrix."
  ;; Matrix -> Vector
  (car m))

(defun first-column (m)
  "Gets the first column of a matrix(ie contents of a Matrix structure)."
  ;; Matrix -> List
  (mapcar #'car m))

(defun getcdr (m)
  "Gets the rest of the columns."
  ;; Matrix -> Matrix
  (mapcar #'cdr m))

(defun get-row (m)
  "Gets the row of a matrix."
  ;; Matrix -> Vector
  (car m))

(defun get-column (m)
  "Gets the first column of the matrix."
  ;; Matrix -> Vector
  (mapcar #'car m))

(defun add-entries (v)
  "Adds the entries in a vector."
  ;; Vector -> Number
  (if (null v)
      0
    (+ (first-entry v) (add-entries (rest-entries v)))))

(defun first-entry (v)
  "Get the first entry of the vector."
  ;; Vector -> Number
  (car v))

(defun rest-entries (v)
  "Get the rest of the vector(ie minus the first entry)."
  (cdr v))

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

(defun inverse (matrix)
  "Computes the inverse of the given matrix."
  ;; Matrix -> Matrix
  ;; given: (inverse '((-1 -2 2) (2 1 1) (3 4 5)))
  ;; expect: `((1/23 18/23 -4/23) (-7/23 -11/23, 5/23), (5/23, -2/23 3/23))
  (let* ((minors (make-matrix-of-minors matrix 1 1))
         (det (determinant matrix 1))
         (adjugate (transpose minors)))
    (multiply-by-scalar (/ 1 det) adjugate)))

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

(defun make-matrix-of-minors (matrix row column)
  "Makes a matrix comprised of cofactors with respect to each element in
   the matrix."
  (if (> row (length matrix))
      '()
    (cons (minor-of-row matrix row column)
	  (make-matrix-of-minors matrix (1+ row) column))))

(defun minor-of-row (matrix row column)
  "Takes a vector of the matrix(eg '(2 3 4)) and computes the cofactor with
   respect to the given matrix of each element in the vector."
  (if (> column (length (car matrix)))
      '()
    (cons (cofactor matrix row column)
	  (minor-of-row matrix row (1+ column)))))

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

;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: multidimensional arrays
;;;Date: Sun Jul  5, 2009
;;;
;;;Abstract
;;;
;;;	This  library is  inspired by  SRFI 25  "Multi-dimensional Array
;;;	Primitives ",  by Jussi Piitulainen.  However,  this library was
;;;	written from scratch.
;;;
;;;	  In  the  comments  of  this  library "items"  are  the  values
;;;	collected in Scheme's built-in lists and vectors, "elements" are
;;;	the values collected in the arrays.
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(library (arrays)
  (export

    ;; array positions
    array-position
    array-position?
    assert-array-position
    assert-array-position/or-false
    array-position=?
    array-position-number-of-dimensions
    array-position-difference
    array-position-step
    array-position-step!
    array-position->string
    array-position-display
    array-position-write

    ;; array shape
    array-shape
    array-shape?
    array-shape-contains?
    assert-array-shape
    assert-array-shape/or-false
    array-shape=?
    array-supershape?
    array-supershape?/strict
    array-subshape?
    array-subshape?/strict
    array-shape-number-of-dimensions
    array-shape-number-of-elements
    array-shape->string
    array-shape-display
    array-shape-write

    ;; arrays
    make-array
    array
    array?
    array-copy
    array-ref
    array-set!
    ;;;array-view
    array->string
    array-display
    array-write
    )
  (import (nausicaa)
    (lists)
    (vectors)
    (strings))


;;;; array position

(define-record-type (:position :position-make array-position?)
  (opaque #t)
  (fields (mutable coordinates)
	  (immutable number-of-dimensions array-position-number-of-dimensions)))
		;a list holding the coordinates

;;; --------------------------------------------------------------------
;;; constructors

(define (array-position . coordinates)
  (unless (every (lambda (n)
		   (and (integer? n) (non-negative? n)))
	    coordinates)
    (assertion-violation 'array-position
      "array coordinates must be non-negative exact integers"
      coordinates))
  (:position-make coordinates (length coordinates)))

;;; --------------------------------------------------------------------
;;; predicates and assertions

(define (assert-array-position obj func-name)
  (or (array-position? obj)
      (assertion-violation func-name
	"expected array position"
	obj)))

(define (assert-array-position/or-false obj func-name)
  (or (or (not obj) (array-position? obj))
      (assertion-violation func-name
	"expected array position"
	obj)))

;;; --------------------------------------------------------------------
;;; comparison

(define (array-position=? position-a position-b)
  (and (= (array-position-number-of-dimensions position-a)
	  (array-position-number-of-dimensions position-b))
       (every =
	 (:position-coordinates position-a)
	 (:position-coordinates position-b))))

;;; --------------------------------------------------------------------
;;; operations

(define (array-position-difference position-a position-b)
  (map -
    (:position-coordinates position-a)
    (:position-coordinates position-b)))

(define (array-position-step position step)
  (:position-make (map + (:position-coordinates position) step)
		  (array-position-number-of-dimensions position)))

(define (array-position-step! position step)
  (:position-coordinates-set! position (map + (:position-coordinates position) step)))

;;; --------------------------------------------------------------------
;;; Conversion and port output.

(define (array-position->string position)
  (string-append "#<array-position -- "
		 (string-join (map number->string (:position-coordinates position)) " ")
		 ">"))

(define array-position-display
  (case-lambda
   ((position)
    (array-position-display position (current-output-port)))
   ((position port)
    (display (array-position->string position) port))))

(define array-position-write
  (case-lambda
   ((position)
    (array-position-write position (current-output-port)))
   ((position port)
    (write (cons 'array-position (:position-coordinates position)) port))))


;;;; array shape

(define-record-type (:shape :shape-make array-shape?)
  (opaque #t)
  (fields (immutable starts)
		;A list holding the start indexes for each dimension.
	  (immutable pasts)))
		;A list holding the past indexes for each dimension.

;;; --------------------------------------------------------------------
;;; constructors

(define (array-shape starts pasts)
  (when (or (= 0 (length starts))
	    (not (= (length starts) (length pasts))))
    (assertion-violation 'array-shape
      "invalid number of elements in shape specification"
      starts pasts))
  (let loop ((starts starts)
	     (pasts  pasts))
    (cond ((null? starts)
	   #t)
	  ((let ((start (car starts))
		 (past  (car pasts)))
	     (and (integer? start) (non-negative? start)
		  (integer? past)  (non-negative? past)
		  (< start past)))
	   (loop (cdr starts) (cdr pasts)))
	  (else
	   (assertion-violation 'array-shape
	     "invalid elements in shape specification"
	     (car starts) (car pasts) starts pasts))))
  (:shape-make starts pasts))

;;; --------------------------------------------------------------------
;;; predicates and assertions

(define (array-shape-contains? shape position)
  (every (lambda (start past index)
	   (and (<= start index) (< index past)))
    (:shape-starts shape)
    (:shape-pasts  shape)
    (:position-coordinates position)))

(define (assert-array-shape obj func-name)
  (or (array-shape? obj)
      (assertion-violation func-name
	"expected array shape"
	obj)))

(define (assert-array-shape/or-false obj func-name)
  (or (or (not obj) (array-shape? obj))
      (assertion-violation func-name
	"expected array shape"
	obj)))

;;; --------------------------------------------------------------------
;;; inspection

(define (array-shape-number-of-dimensions shape)
  (length (:shape-starts shape)))

(define (array-shape-number-of-elements shape)
  (fold-left (lambda (sum start past)
	       (+ sum (- past start)))
	     0
	     (:shape-starts shape)
	     (:shape-pasts shape)))

(define (array-shape-index-start shape dimension)
  (list-index (:shape-starts) dimension))

(define (array-shape-index-past shape dimension)
  (list-index (:shape-pasts) dimension))

(define (array-shape-index-last shape dimension)
  (- (list-index (:shape-pasts) dimension) 1))

;;; --------------------------------------------------------------------
;;; comparison

(define array-shape=?
  (case-lambda
   ((shape-a shape-b)
    (and (= (array-shape-number-of-dimensions shape-a)
	    (array-shape-number-of-dimensions shape-b))
	 (every (lambda (sa sb pa pb) (and (= sa sb) (= pa pb)))
	   (:shape-starts shape-a)
	   (:shape-starts shape-b)
	   (:shape-pasts shape-a)
	   (:shape-pasts shape-b))))
   ((shape0 . shape-args)
    (fold-left/pred array-shape? (cons shape0 shape-args)))))

(define array-supershape?
  (case-lambda
   ((shape-a shape-b)
    (and (= (array-shape-number-of-dimensions shape-a)
	    (array-shape-number-of-dimensions shape-b))
	 (every (lambda (sa sb pa pb) (and (<= sa sb) (<= pb pa)))
	   (:shape-starts shape-a)
	   (:shape-starts shape-b)
	   (:shape-pasts shape-a)
	   (:shape-pasts shape-b))))
   ((shape0 . shape-args)
    (fold-left/pred array-supershape? shape0 shape-args))))

(define array-supershape?/strict
  (case-lambda
   ((shape-a shape-b)
    (and (= (array-shape-number-of-dimensions shape-a)
	    (array-shape-number-of-dimensions shape-b))
	 (let loop ((one-is-strict-supershape? #f)
		    (starts-a	(:shape-starts shape-a))
		    (starts-b	(:shape-starts shape-b))
		    (pasts-a	(:shape-pasts shape-a))
		    (pasts-b	(:shape-pasts shape-b)))
	   (cond ((null? starts-a)
		  one-is-strict-supershape?)
		 ((and (<  (car starts-a) (car starts-b))
		       (<= (car  pasts-b) (car  pasts-a)))
		  (loop #t
			(cdr starts-a) (cdr starts-b)
			(cdr  pasts-a) (cdr  pasts-b)))
		 ((and (<= (car starts-a) (car starts-b))
		       (<  (car  pasts-b) (car  pasts-a)))
		  (loop #t
			(cdr starts-a) (cdr starts-b)
			(cdr  pasts-a) (cdr  pasts-b)))
		 ((and (<= (car starts-a) (car starts-b))
		       (<= (car  pasts-b) (car  pasts-a)))
		  (loop one-is-strict-supershape?
			(cdr starts-a) (cdr starts-b)
			(cdr  pasts-a) (cdr  pasts-b)))
		 (else #f)))))
   ((shape0 . shape-args)
    (fold-left/pred array-supershape?/strict shape0 shape-args))))

(define (array-subshape? shape0 . shapes)
  (apply array-supershape? (reverse (cons shape0 shapes))))

(define (array-subshape?/strict shape0 . shapes)
  (apply array-supershape?/strict (reverse (cons shape0 shapes))))

;;; --------------------------------------------------------------------
;;; Conversion and port output

(define (array-shape->string shape)
  (string-append "#<array-shape -- "
		 (string-join (map number->string (:shape-starts shape)) " ")
		 " -- "
		 (string-join (map number->string (:shape-pasts  shape)) " ")
		 ">"))

(define array-shape-display
  (case-lambda
   ((shape)
    (array-shape-display shape (current-output-port)))
   ((shape port)
    (display (array-shape->string shape) port))))

(define array-shape-write
  (case-lambda
   ((shape)
    (array-shape-write shape (current-output-port)))
   ((shape port)
    (display "(array-shape '" port)
    (write (:shape-starts shape) port)
    (display " '" port)
    (write (:shape-pasts shape) port)
    (display ")" port))))


;;;; arrays

(define-record-type (:array :array-make array?)
  (opaque #t)
  (parent :shape)
  (fields (immutable dimensions)
	  (immutable factors)
	  (immutable vector)))

;;; --------------------------------------------------------------------
;;; constructors

(define (%compute-factors dimensions)
  (let loop ((dims    (cdr (reverse dimensions)))
	     (factors '(1)))
    (if (null? dims)
	factors
      (loop (cdr dims)
	    (cons (apply * dims) factors)))))

(define make-array
  (case-lambda
   ((shape)
    (make-array shape #f))
   ((shape fill-value)
    (let* ((starts	(:shape-starts shape))
	   (pasts	(:shape-pasts  shape))
	   (dimensions	(map - pasts starts)))
      (:array-make starts pasts dimensions
		   (%compute-factors dimensions)
		   (make-vector (apply * dimensions) fill-value))))))

(define (array shape . elements)
  (let* ((array (make-array shape))
	 (vec   (:array-vector array))
	 (len   (vector-length vec)))
    (do ((i 0 (+ 1 i))
	 (elements elements (begin
			      (when (null? elements)
				(assertion-violation 'array
				  "number of elements less than size of array"))
			      (cdr elements))))
	((= i len)
	 (unless (null? elements)
	   (assertion-violation 'array
	     "number of elements exceeds size of array"))
	 array)
      (vector-set! vec i (car elements)))))

(define array-copy
  (case-lambda
   ((array)
    (:array-make (:shape-starts array)
		 (:shape-pasts  array)
		 (vector-copy (:array-vector array))))
   ((array shape)
    (:array-make (make-list (length (:shape-starts array)) 0)
		 (map - (:shape-pasts shape) (:shape-starts shape))
		 (let ((src (:array-vector array))
		       (dst (make-vector (array-shape-number-of-elements shape)))
		       (i   0))
		   (map (lambda (start past)
			  (do ((j start (+ 1 j)))
			      ((= j past))
			    (vector-set! dst i (vector-ref src j))
			    (set! i (+ 1 i))))
		     (:shape-starts array)
		     (:shape-pasts  array))
		   dst)))))

;;; --------------------------------------------------------------------
;;; predicates and assertions

(define (assert-array obj func-name)
  (or (array? obj)
      (assertion-violation func-name
	"expected array" obj)))

(define (assert-array/or-false obj func-name)
  (or (or (not obj) (array? obj))
      (assertion-violation func-name
	"expected array" obj)))

;;; --------------------------------------------------------------------
;;; comparison

(define array=?
  (case-lambda
   ((item= array-a array-b)
    (and (array-shape=? array-a array-b)
	 (vector= item=
		  (:array-vector array-a)
		  (:array-vector array-b))))
   ((item= array0 . array-args)
    (fold-left/pred array=? (cons array0 array-args)))))

;;; --------------------------------------------------------------------
;;; accessors

(define (%compute-index proc-name array position)
  (write (list 'factors (:array-factors array)))(newline)
  (fold-left (lambda (offset factor index)
	       (+ offset (* factor index)))
	     0
	     (:array-factors array)
	     (:position-coordinates position)))

(define (array-ref array position)
;;;  (write (:array-vector array))(newline)
  (vector-ref (:array-vector array)
	      (%compute-index 'array-ref array position)))

(define (array-set! array position value)
  (vector-set! (:array-vector array)
	       (%compute-index 'array-set! array position)
	       value))

;;; --------------------------------------------------------------------
;;; Conversion and port output

(define (array->string element->string array)
  (string-append "#<array "
		 (array-shape->string array)
		 " "
		 (vector-fold-right
		  (lambda (idx string item)
		    (string-append (element->string item) " " string))
		  ""
		  (:array-vector array))
		 ">"))

(define array-display
  (case-lambda
   ((element->string array)
    (array-display element->string array (current-output-port)))
   ((element->string array port)
    (display (array->string element->string array) port))))

(define array-write
  (case-lambda
   ((element->string array)
    (array-write element->string array (current-output-port)))
   ((element->string array port)
    (display "(array " port)
    (array-shape-write array port)
    (display " " port)
    (display (vector-fold-right
	      (lambda (idx string item)
		(string-append (element->string item) " " string))
	      ""
	      (:array-vector array))
	     port)
    (display ")" port))))


;;;; views

;; (define (array-view array shape)
;;   (if (array-and-shape? array shape)
;;       (:array-make (:shape-starts shape)
;; 		   (:shape-pasts  shape)
;; 		   (:array-vector array))
;;     (assertion-violation 'array-view
;;       "invalid shape to build a view over array"
;;       array shape)))


;;;; done

)

;;; end of file

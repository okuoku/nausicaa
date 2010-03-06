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
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    array-view
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


;;;; helpers

(define (%coordinate? num)
  (and (integer? num) (exact? num)))


;;;; array position

;;; constructors

(define (array-position . coordinates)
  (unless (every %coordinate? coordinates)
    (assertion-violation 'array-position
      "array coordinates must be non-negative exact integers"
      coordinates))
  (list->vector coordinates))

;;; --------------------------------------------------------------------
;;; predicates and assertions

(define (array-position? position)
  (and (vector? position)
       (vector-every %coordinate? position)
       #t))

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
;;; Conversion and port output.

(define (array-position->string position)
  (string-append "#<array-position -- "
		 (string-join (vector->list (vector-map number->string position)) " ")
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
    (write position port))))


;;;; array shape

(define-record-type (:shape :shape-make array-shape?)
  (opaque #t)
  (fields (immutable starts)
		;A vector holding the start indexes for each dimension.
	  (immutable pasts)))
		;A vector holding the past indexes for each dimension.

;;; --------------------------------------------------------------------
;;; constructors

(define (array-shape starts pasts)
  (let ((len (vector-length starts)))
    (when (or (zero? len) (not (= len (vector-length pasts))))
      (assertion-violation 'array-shape
	"invalid number of elements in shape specification"
	starts pasts))
    (do* ((i 0 (+ 1 i))
	  (S (vector-ref starts i) (vector-ref starts i))
	  (P (vector-ref pasts  i) (vector-ref pasts  i)))
	((= i len))
      (unless (%coordinate? S)
	(assertion-violation 'array-shape
	  "invalid elements in shape starts specification"
	  S))
      (unless (%coordinate? P)
	(assertion-violation 'array-shape
	  "invalid elements in shape pasts specification"
	  P))))
  (:shape-make starts pasts))

;;; --------------------------------------------------------------------
;;; predicates and assertions

(define (array-shape-contains? shape position)
  (vector-every (lambda (start past index)
		  (and (<= start index) (< index past)))
    (:shape-starts shape)
    (:shape-pasts  shape)
    position))

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
  (vector-length (:shape-starts shape)))

(define (array-shape-number-of-elements shape)
  (vector-fold-left (lambda (sum start past)
		      (+ sum (- past start)))
    0
    (:shape-starts shape)
    (:shape-pasts shape)))

(define (array-shape-index-start shape dimension)
  (vector-ref (:shape-starts) dimension))

(define (array-shape-index-past shape dimension)
  (vector-ref (:shape-pasts) dimension))

(define (array-shape-index-last shape dimension)
  (- (vector-ref (:shape-pasts) dimension) 1))

;;; --------------------------------------------------------------------
;;; comparison

(define array-shape=?
  (case-lambda
   ((shape-a shape-b)
    (and (= (array-shape-number-of-dimensions shape-a)
	    (array-shape-number-of-dimensions shape-b))
	 (vector-every (lambda (sa sb pa pb) (and (= sa sb) (= pa pb)))
	   (:shape-starts shape-a)
	   (:shape-starts shape-b)
	   (:shape-pasts shape-a)
	   (:shape-pasts shape-b))))
   ((shape0 . shape-args)
    (vector-fold-left/pred array-shape? (cons shape0 shape-args)))))

(define array-supershape?
  (case-lambda
   ((shape-a shape-b)
    (and (= (array-shape-number-of-dimensions shape-a)
	    (array-shape-number-of-dimensions shape-b))
	 (vector-every (lambda (sa sb pa pb) (and (<= sa sb) (<= pb pa)))
	   (:shape-starts shape-a)
	   (:shape-starts shape-b)
	   (:shape-pasts shape-a)
	   (:shape-pasts shape-b))))
   ((shape0 . shape-args)
    (vector-fold-left/pred array-supershape? shape0 shape-args))))

(define array-supershape?/strict
  (case-lambda
   ((shape-a shape-b)
    (and (= (array-shape-number-of-dimensions shape-a)
	    (array-shape-number-of-dimensions shape-b))
	 (let* ((starts-a	(:shape-starts shape-a))
		(starts-b	(:shape-starts shape-b))
		(pasts-a	(:shape-pasts  shape-a))
		(pasts-b	(:shape-pasts  shape-b))
		(len		(vector-length starts-a)))
	   (let loop ((one-is-strict-supershape? #f)
		      (i 0))
	     (if (= i len)
		 one-is-strict-supershape?
	       (let ((Sa (vector-ref starts-a i))
		     (Sb (vector-ref starts-b i))
		     (Pa (vector-ref pasts-a  i))
		     (Pb (vector-ref pasts-b  i)))
		 (cond ((null? starts-a)
			one-is-strict-supershape?)
		       ((and (<  Sa Sb) (<= Pb Pa))
			(loop #t (+ 1 i)))
		       ((and (<= Sa Sb) (<  Pb Pa))
			(loop #t (+ 1 i)))
		       ((and (<= Sa Sb) (<= Pb Pa))
			(loop one-is-strict-supershape? (+ 1 i)))
		       (else #f))))))))
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
		 (string-join (vector->list (vector-map number->string (:shape-starts shape))) " ")
		 " -- "
		 (string-join (vector->list (vector-map number->string (:shape-pasts  shape))) " ")
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
		;a vector holding the lengths of the dimensions
	  (immutable factors)
		;a  vector  holding  the  factors used  to  compute  the
		;absolute index in the underlying vector
	  (immutable mapper)
		;a mapper function for coordinates
	  (immutable vector)))
		;the underlying vector

;;; --------------------------------------------------------------------
;;; constructors

(define (%compute-factors dimensions)
  (do* ((len (vector-length dimensions))
	(factors (make-vector len))
	(kmax (- len 1))
	(k 0 (+ 1 k)))
      ((= k kmax)
       (vector-set! factors (- len 1) 1)
       factors)
    (vector-set! factors k
		 (subvector-fold-left * 1 (view dimensions (start (+ 1 k)))))))

;; (define (%compute-factors dimensions)
;;   (let loop ((dims    (cdr dimensions))
;; 	     (factors '()))
;;     (if (null? dims)
;; 	(reverse (cons 1 factors))
;;       (loop (cdr dims)
;; 	    (cons (apply * dims) factors)))))

(define make-array
  (case-lambda
   ((shape)
    (make-array shape #f))
   ((shape fill-value)
    (let* ((starts	(:shape-starts shape))
	   (pasts	(:shape-pasts  shape))
	   (dimensions	(vector-map - pasts starts)))
      (:array-make starts pasts dimensions
		   (%compute-factors dimensions)
		   #f
		   (make-vector (vector-fold-left * 1 dimensions) fill-value))))))

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

(define (array-copy array)
  (:array-make (vector-copy (:shape-starts array))
	       (vector-copy (:shape-pasts  array))
	       (vector-copy (:array-dimensions array))
	       (vector-copy (:array-factors array))
	       (vector-copy (:array-mapper array))
	       (vector-copy (:array-vector array))))

(define (array-view array mapper)
  (:array-make (:shape-starts array)
	       (:shape-pasts  array)
	       (:array-dimensions array)
	       (:array-factors array)
	       (let ((under (:array-mapper array)))
		 (if under
		     (lambda (position)
		       (under (mapper position)))
		   mapper))
	       (:array-vector array)))

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
    (vector-fold-left/pred array=? (cons array0 array-args)))))

;;; --------------------------------------------------------------------
;;; accessors

(define (%compute-index proc-name array position)
  (vector-fold-left (lambda (offset factor index)
		      (+ offset (* factor index)))
    0
    (:array-factors array)
    (let ((mapper (:array-mapper array)))
      (if mapper
	  (mapper position)
	position))))

(define (array-ref array position)
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
		  (lambda (item string)
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
	      (lambda (item string)
		(string-append (element->string item) " " string))
	      ""
	      (:array-vector array))
	     port)
    (display ")" port))))


;;;; done

)

;;; end of file

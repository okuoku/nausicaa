;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: precompute values for the (random) library
;;;Date: Fri Jun 26, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (c) 2008 Derick Eddington
;;;Copyright (c) 2002 Sebastian Egner
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


(import (rnrs)
  (rnrs r5rs))

(define mrg32k3a-m1 4294967087) ; modulus of component 1
(define mrg32k3a-m2 4294944443) ; modulus of component 2

(define (product A B)	; A*B in ((Z/m1*Z) x (Z/m2*Z))^(3x3)
  (define w      65536)	; wordsize to split {0..2^32-1}
  (define w-sqr1 209)	; w^2 mod m1
  (define w-sqr2 22853)	; w^2 mod m2
  (define (lc i0 i1 i2 j0 j1 j2 m w-sqr) ; linear combination
    (let ((a0h (quotient (vector-ref A i0) w))
	  (a0l (modulo   (vector-ref A i0) w))
	  (a1h (quotient (vector-ref A i1) w))
	  (a1l (modulo   (vector-ref A i1) w))
	  (a2h (quotient (vector-ref A i2) w))
	  (a2l (modulo   (vector-ref A i2) w))
	  (b0h (quotient (vector-ref B j0) w))
	  (b0l (modulo   (vector-ref B j0) w))
	  (b1h (quotient (vector-ref B j1) w))
	  (b1l (modulo   (vector-ref B j1) w))
	  (b2h (quotient (vector-ref B j2) w))
	  (b2l (modulo   (vector-ref B j2) w)))
      (modulo
       (+ (* (+ (* a0h b0h)
		(* a1h b1h)
		(* a2h b2h))
	     w-sqr)
	  (* (+ (* a0h b0l)
		(* a0l b0h)
		(* a1h b1l)
		(* a1l b1h)
		(* a2h b2l)
		(* a2l b2h))
	     w)
	  (* a0l b0l)
	  (* a1l b1l)
	  (* a2l b2l))
       m)))

  (vector
   (lc  0  1  2   0  3  6  mrg32k3a-m1 w-sqr1) ; (A*B)_00 mod m1
   (lc  0  1  2   1  4  7  mrg32k3a-m1 w-sqr1) ; (A*B)_01
   (lc  0  1  2   2  5  8  mrg32k3a-m1 w-sqr1)
   (lc  3  4  5   0  3  6  mrg32k3a-m1 w-sqr1) ; (A*B)_10
   (lc  3  4  5   1  4  7  mrg32k3a-m1 w-sqr1)
   (lc  3  4  5   2  5  8  mrg32k3a-m1 w-sqr1)
   (lc  6  7  8   0  3  6  mrg32k3a-m1 w-sqr1)
   (lc  6  7  8   1  4  7  mrg32k3a-m1 w-sqr1)
   (lc  6  7  8   2  5  8  mrg32k3a-m1 w-sqr1)
   (lc  9 10 11   9 12 15  mrg32k3a-m2 w-sqr2) ; (A*B)_00 mod m2
   (lc  9 10 11  10 13 16  mrg32k3a-m2 w-sqr2)
   (lc  9 10 11  11 14 17  mrg32k3a-m2 w-sqr2)
   (lc 12 13 14   9 12 15  mrg32k3a-m2 w-sqr2)
   (lc 12 13 14  10 13 16  mrg32k3a-m2 w-sqr2)
   (lc 12 13 14  11 14 17  mrg32k3a-m2 w-sqr2)
   (lc 15 16 17   9 12 15  mrg32k3a-m2 w-sqr2)
   (lc 15 16 17  10 13 16  mrg32k3a-m2 w-sqr2)
   (lc 15 16 17  11 14 17  mrg32k3a-m2 w-sqr2))) ; end of PRODUCT

(define (power A e) ; A^e
  (cond
   ((zero? e)
    '#(1 0 0 0 1 0 0 0 1 1 0 0 0 1 0 0 0 1))
   ((= e 1)
    A)
   ((even? e)
    (power (product A A) (quotient e 2)))
   (else
    (product (power A (- e 1)) A))))

(define (power-power A b) ; A^(2^b)
  (if (zero? b)
      A
    (power-power (product A A) (- b 1))))

(define A	; the MRG32k3a recursion
  '#(0 1403580 4294156359
     1       0          0
     0       1          0
     527612  0 4293573854
     1       0          0
     0       1          0))

;; precompute A^(2^127) and A^(2^76) only once
(define mrg32k3a-generators-0 (power-power A 127))
(define mrg32k3a-generators-1 (power-power A  76))
(define mrg32k3a-generators-2 (power       A  16))

(write (list 'mrg32k3a-generators-0 (power-power A 127)))(newline)
(write (list 'mrg32k3a-generators-1 (power-power A  76)))(newline)
(write (list 'mrg32k3a-generators-2 (power       A  16)))(newline)

;;; end of file
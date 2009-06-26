;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record interface to randomness sources
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


#!r6rs
(library (random)
  (export

    ;; device-based random sources of numbers
    %random-bytevector!		%random-bytevector
     random-bytevector!		 random-bytevector
    urandom-bytevector!		urandom-bytevector

    ;; simple sources of numbers
    make-random-source-of-integers/device
    random-source-of-integers

    ;; no fuss
    random-integer random-real

    ;; random source
    default-random-source
    make-random-source
    random-source?
    random-source-state-ref      random-source-state-set!
    random-source-randomize!     random-source-pseudo-randomize!
    random-source-integers-maker  random-source-reals-maker

    ;; utilities
    unfold-random-numbers
    unfold-random-numbers/vector  unfold-random-numbers/string
    random-source-make-permutations
    random-source-make-exponentials
    random-source-make-normals)
  (import (rnrs)
    (nausicaa parameters)
    (rnrs mutable-strings)
    (only (rnrs r5rs)
	  quotient modulo))


;;;; helpers

(define (vector-copy src)
  (let* ((len (vector-length src))
	 (dst (make-vector len)))
    (do ((i 0 (+ 1 i)))
	((= len i)
	 dst)
      (vector-set! dst i (vector-ref src i)))))


;;;; randomness from devices

(define (%random-bytevector! device bv)
  (let* ((number-of-bytes (bytevector-length bv))
	 (port (open-file-input-port device
				     (file-options no-create)
				     (buffer-mode block))))
    (dynamic-wind
	(lambda () #f)
	(lambda () (get-bytevector-n! port bv 0 number-of-bytes))
	(lambda () (close-port port))))
  bv)

(define (%random-bytevector device number-of-bytes)
  (%random-bytevector! device (make-bytevector number-of-bytes)))

;;; --------------------------------------------------------------------

(define (random-bytevector! bv)
  (%random-bytevector! "/dev/random" bv))

(define (random-bytevector number-of-bytes)
  (%random-bytevector "/dev/random" number-of-bytes))

(define (urandom-bytevector! bv)
  (%random-bytevector! "/dev/urandom" bv))

(define (urandom-bytevector number-of-bytes)
  (%random-bytevector "/dev/urandom" number-of-bytes))


;;;; simple sources of integers

(define make-random-source-of-integers/device
  (case-lambda
   (()
    (make-random-source-of-integers/device "/dev/urandom"))
   ((device)
    (let* ((cache (make-bytevector 4096))
	   (next  4096))
		;This init value  causes the vector to be  filled at the
		;first invocation.
      (lambda ()
	(when (<= (bytevector-length cache) next)
	  (%random-bytevector! device cache)
;;;	  (write (list 'random-bytevector cache))(newline)
	  (set! next 0))
	(let ((n (bytevector-u32-native-ref cache next)))
	  (set! next (+ 4 next))
;;;	  (write (list 'number n))(newline)
	  n))))))

(define random-source-of-integers
  (make-parameter
      (make-random-source-of-integers/device)
    (lambda (obj)
      (if (procedure? obj)
	  obj
	(assertion-violation 'random-source-of-integers
	  "expected procedure" obj)))))



(define-record-type (:random-source :random-source-make random-source?)
  (fields (immutable state-ref)
	  (immutable state-set!)
	  (immutable randomize!)
	  (immutable pseudo-randomize!)
	  (immutable integers-maker)
	  (immutable reals-maker)))

(define (random-source-state-ref s)
  ((:random-source-state-ref s)))

(define (random-source-state-set! s state)
  ((:random-source-state-set! s) state))

(define (random-source-randomize! s)
  ((:random-source-randomize! s)))

(define (random-source-pseudo-randomize! s i j)
  ((:random-source-pseudo-randomize! s) i j))

(define (random-source-integers-maker s)
  ((:random-source-integers-maker s)))

(define random-source-reals-maker
  (case-lambda
   ((source)
    ((:random-source-reals-maker source)))
   ((source unit)
    ((:random-source-reals-maker source) unit))))


;;; MRG32k3a pseudo-random numbers generator

(define (make-random-source)
  (let ((state (vector-copy mrg32k3a-initial-state)))
    (:random-source-make
     (lambda () ; state-ref
       (mrg32k3a-state-internal->external state))
     (lambda (new-state) ; state-set!
       (set! state (mrg32k3a-state-external->internal new-state)))
     (lambda () ; randomize!
       (set! state (mrg32k3a-randomize-state state)))
     (lambda (i j) ; pseudo-randomize!
       (set! state (mrg32k3a-pseudo-randomize-state i j)))
     (lambda () ; integers-maker
       (lambda (n)
         (cond ((not (and (integer? n) (exact? n) (positive? n)))
		(assertion-violation 'integers-maker
		  "range upper limit must be an exact positive integer" n))
	       ((<= n mrg32k3a-m-max)
		(mrg32k3a-random-integer state n))
	       (else
		(mrg32k3a-random-large state n)))))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (mrg32k3a-random-real state)))
      ((unit)
       (cond ((not (and (real? unit) (< 0 unit 1)))
	      (assertion-violation 'reals-maker
		"unit must be a real number in the range (0,1)" unit))
	     ((<= (- (/ 1 unit) 1) mrg32k3a-m1)
	      (lambda ()
		(mrg32k3a-random-real state)))
	     (else
	      (lambda ()
		(mrg32k3a-random-real-mp state unit)))))))))

;;; --------------------------------------------------------------------

(define mrg32k3a-m1 4294967087) ; modulus of component 1
(define mrg32k3a-m2 4294944443) ; modulus of component 2
(define e2^28 (expt 2 28))

(define mrg32k3a-initial-state ; 0 3 6 9 12 15 of A^16, see below
  '#(1062452522
     2961816100
     342112271
     2854655037
     3321940838
     3542344109))

(define (mrg32k3a-random-m1 state)
  (let ((x11 (vector-ref state 0))
        (x12 (vector-ref state 1))
        (x13 (vector-ref state 2))
        (x21 (vector-ref state 3))
        (x22 (vector-ref state 4))
        (x23 (vector-ref state 5)))
    (let ((x10 (modulo (- (* 1403580 x12) (* 810728 x13)) 4294967087))
          (x20 (modulo (- (* 527612 x21) (* 1370589 x23)) 4294944443)))
      (vector-set! state 0 x10)
      (vector-set! state 1 x11)
      (vector-set! state 2 x12)
      (vector-set! state 3 x20)
      (vector-set! state 4 x21)
      (vector-set! state 5 x22)
      (modulo (- x10 x20) 4294967087))))

(define (mrg32k3a-random-range) ; m1
  4294967087)

(define (mrg32k3a-random-integer state range) ; rejection method
  (let* ((q  (quotient 4294967087 range))
         (qn (* q range)))
    (do ((x (mrg32k3a-random-m1 state) (mrg32k3a-random-m1 state)))
      ((< x qn) (quotient x q)))))

(define (mrg32k3a-random-real state) ; normalization is 1/(m1+1)
  (* 0.0000000002328306549295728 (+ 1.0 (mrg32k3a-random-m1 state))))

;;; --------------------------------------------------------------------
;;; state accessors

(define (mrg32k3a-state-internal->external state)
  ;;Package the state to be written in a way that can be read back.
  (cons 'lecuyer-mrg32k3a state))

(define (mrg32k3a-state-external->internal external-state)
  ;;Given a packaged state, verifies it and return an internal state.
  (define (check-value x m)
    (or (and (integer? x)
             (exact? x)
             (<= 0 x (- m 1)))
	(assertion-violation 'mrg32k3a-state-external->internal
	  "illegal state value" (list x external-state))))
  (unless (and (pair? external-state)
	       (eq? (car external-state) 'lecuyer-mrg32k3a)
	       (vector? (cdr external-state))
	       (= 6 (vector-length (cdr external-state))))
    (assertion-violation 'mrg32k3a-state-external->internal
      "invalid external state argument" external-state))
  (let* ((state (cdr external-state))
	 (s0 (vector-ref state 0))
	 (s1 (vector-ref state 1))
	 (s2 (vector-ref state 2))
	 (s3 (vector-ref state 3))
	 (s4 (vector-ref state 4))
	 (s5 (vector-ref state 5)))
    (check-value s0 mrg32k3a-m1)
    (check-value s1 mrg32k3a-m1)
    (check-value s2 mrg32k3a-m1)
    (check-value s3 mrg32k3a-m2)
    (check-value s4 mrg32k3a-m2)
    (check-value s5 mrg32k3a-m2)
    (when (or (zero? (+ s0 s1 s2))
	      (zero? (+ s3 s4 s5)))
      (assertion-violation 'mrg32k3a-state-external->internal
	"illegal degenerate state" external-state))
    (vector-copy state)))

;;; --------------------------------------------------------------------
;;; pseudo-randomization

;;Precomputed A^(2^127) and A^(2^76).  The values were precomputed using
;;the  Scheme  program  "mrg32k3a-make-generators.sps" which  should  be
;;included in the source distribution of this package.
;;
;; (define mrg32k3a-generators-0 (power-power A 127))
;; (define mrg32k3a-generators-1 (power-power A  76))
;; (define mrg32k3a-generators-2 (power       A  16))

(define mrg32k3a-generators-0
  '#(1230515664 986791581 1988835001 3580155704
		1230515664 226153695 949770784
		3580155704 2427906178 2093834863
		32183930 2824425944 1022607788
		1464411153 32183930 1610723613
		277697599 1464411153))
(define mrg32k3a-generators-1
  '#(69195019 3528743235 3672091415 1871391091
	      69195019 3672831523 4127413238
	      1871391091 82758667 3708466080
	      4292754251 3859662829 3889917532
	      1511326704 4292754251 1610795712
	      3759209742 1511326704))
(define mrg32k3a-generators-2
  '#(1062452522 340793741 2955879160 2961816100
		1062452522 387300998 342112271
		2961816100 736416029 2854655037
		1817134745 3493477402 3321940838
		818368950 1817134745 3542344109
		3790774567 818368950))

(define (mrg32k3a-pseudo-randomize-state i j)
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

  ;; check arguments
  (unless (and (integer? i) (exact? i)
	       (integer? j) (exact? j))
    (assertion-violation 'mrg32k3a-pseudo-randomize-state
      "pseudo-randomisation arguments must be exact integers" i j))

		; compute M = A^(16 + i*2^127 + j*2^76)
  (let ((M (product mrg32k3a-generators-2
		    (product (power mrg32k3a-generators-0 (modulo i e2^28))
			     (power mrg32k3a-generators-1 (modulo j e2^28))))))
    (vector (vector-ref M 0)
	    (vector-ref M 3)
	    (vector-ref M 6)
	    (vector-ref M 9)
	    (vector-ref M 12)
	    (vector-ref M 15))))

;;; --------------------------------------------------------------------
;;; true randomization

(define (mrg32k3a-randomize-state state)
  ;; G. Marsaglia's simple 16-bit generator with carry
  (let* ((m 65536)
         (x (modulo ((random-source-of-integers)) m)))
    (define (random-m)
      (let ((y (modulo x m)))
        (set! x (+ (* 30903 y) (quotient x m)))
        y))
    (define (random n) ; m < n < m^2
      (modulo (+ (* (random-m) m) (random-m)) n))
    (let ((m1 mrg32k3a-m1)
          (m2 mrg32k3a-m2)
          (s  state))
      (vector
       (+ 1 (modulo (+ (vector-ref s 0) (random (- m1 1))) (- m1 1)))
       (modulo (+ (vector-ref s 1) (random m1)) m1)
       (modulo (+ (vector-ref s 2) (random m1)) m1)
       (+ 1 (modulo (+ (vector-ref s 3) (random (- m2 1))) (- m2 1)))
       (modulo (+ (vector-ref s 4) (random m2)) m2)
       (modulo (+ (vector-ref s 5) (random m2)) m2)))))

;;; --------------------------------------------------------------------
;;; numbers making

(define mrg32k3a-m-max
  (mrg32k3a-random-range))

(define (mrg32k3a-random-power state k) ; n = m-max^k, k >= 1
  (let ((n (mrg32k3a-random-integer state mrg32k3a-m-max)))
    (if (= k 1)
	n
      (+ n (* mrg32k3a-m-max (mrg32k3a-random-power state (- k 1)))))))

(define (mrg32k3a-random-large state n) ; n > m-max
  (do ((k 2 (+ k 1))
       (mk (* mrg32k3a-m-max mrg32k3a-m-max) (* mk mrg32k3a-m-max)))
      ((>= mk n)
       (let* ((mk-by-n (quotient mk n))
	      (a (* mk-by-n n)))
	 (do ((x (mrg32k3a-random-power state k)
		 (mrg32k3a-random-power state k)))
	     ((< x a)
	      (quotient x mk-by-n)))))))

(define (mrg32k3a-random-real-mp state unit)
  (do ((k 1 (+ k 1))
       (u (- (/ 1 unit) 1) (/ u mrg32k3a-m1)))
      ((<= u 1)
       (/ (inexact (+ (mrg32k3a-random-power state k) 1))
	  (inexact (+ (expt mrg32k3a-m-max k) 1))))))


;;; no fuss API

(define default-random-source
  (make-random-source))

(define random-integer
  (random-source-integers-maker default-random-source))

(define random-real
  (random-source-reals-maker default-random-source))


;;; utility procedures

(define (unfold-random-numbers source number-of-numbers)
  (do ((i 0 (+ 1 i))
       (ell '() (cons (source) ell)))
      ((= i number-of-numbers)
       ell)))

(define (unfold-random-numbers/vector source number-of-numbers)
  (do ((i 0 (+ 1 i))
       (vec (make-vector number-of-numbers) (begin (vector-set! vec i (source)) vec)))
      ((= i number-of-numbers)
       vec)))

(define (unfold-random-numbers/string source number-of-numbers)
  (let ((str (make-string number-of-numbers)))
    (do ((i 0 (+ 1 i)))
	((= i number-of-numbers)
	 str)
      (do ((n (source) (source)))
	  ((or (and (<= 0 n)     (< n #xD800))
	       (and (< #xDFFF n) (< n #x10FFFF)))
	   (string-set! str i (integer->char n)))))))

(define (random-source-make-permutations source)
  ;;For  the   algorithm  refer  to   Knuth's  ``The  Art   of  Computer
  ;;Programming'', Vol. II, 2nd ed., Algorithm P of Section 3.4.2.
  (let ((rand (random-source-integers-maker source)))
    (lambda (n)
      (let ((x (make-vector n 0)))
        (do ((i 0 (+ i 1)))
            ((= i n))
          (vector-set! x i i))
        (do ((k n (- k 1)))
            ((= k 1) x)
          (let* ((i (- k 1))
                 (j (rand k))
                 (xi (vector-ref x i))
                 (xj (vector-ref x j)))
            (vector-set! x i xj)
            (vector-set! x j xi)))))))

(define (random-source-make-exponentials source . unit)
  ;;Refer to Knuth's  ``The Art of Computer Programming'',  Vol. II, 2nd
  ;;ed., Section 3.4.1.D.
  (let ((rand (apply random-source-reals-maker source unit)))
    (lambda (mu)
      (- (* mu (log (rand)))))))

(define (random-source-make-normals source . unit)
  ;;For  the   algorithm  refer  to   Knuth's  ``The  Art   of  Computer
  ;;Programming'', Vol. II, 2nd ed., Algorithm P of Section 3.4.1.C.
  (let ((rand (apply random-source-reals-maker source unit))
        (next #f))
    (lambda (mu sigma)
      (if next
          (let ((result next))
            (set! next #f)
            (+ mu (* sigma result)))
        (let loop ()
          (let* ((v1 (- (* 2 (rand)) 1))
                 (v2 (- (* 2 (rand)) 1))
                 (s (+ (* v1 v1) (* v2 v2))))
            (if (>= s 1)
                (loop)
              (let ((scale (sqrt (/ (* -2 (log s)) s))))
                (set! next (* scale v2))
                (+ mu (* sigma scale v1))))))))))


;;; done

)

;;; end of file

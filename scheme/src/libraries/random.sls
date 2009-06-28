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
;;;


#!r6rs
(library (random)
  (export

    ;; random source interface
    random-source?
    random-source-state-ref      random-source-state-set!
    random-source-seed!
    random-source-integers-maker random-source-reals-maker
    random-source-bytevectors-maker

    ;; random source constructors
    make-random-source		make-random-source/device

    ;; low level API for device-based random sources of numbers
    random-device-cache-length
    %device-read-bytevector!	%device-read-bytevector
    %device-write-bytevector!
    random-device-bytevector!	random-device-bytevector
    urandom-device-bytevector!	urandom-device-bytevector

    ;; no fuss API
    random-integer		random-real
    default-random-source

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

(define const:2^32		(expt 2 32))
(define const:2^32-1		(- const:2^32 1))
(define const:2^32^2		(* const:2^32 const:2^32))
(define const:1/2^32^2	(/ 1.0 const:2^32^2))

(define (vector-copy src)
  (let* ((len (vector-length src))
	 (dst (make-vector len)))
    (do ((i 0 (+ 1 i)))
	((= len i)
	 dst)
      (vector-set! dst i (vector-ref src i)))))


;;;; randomness source

(define-record-type (:random-source :random-source-make random-source?)
  (fields (immutable state-ref)
	  (immutable state-set!)
	  (immutable seed!)
	  (immutable integers-maker)
	  (immutable reals-maker)
	  (immutable bytevectors-maker)))

(define (random-source-state-ref s)
  ((:random-source-state-ref s)))

(define (random-source-state-set! s state)
  ((:random-source-state-set! s) state))

(define (random-source-seed! s source-of-integers)
  ((:random-source-seed! s) source-of-integers))

(define (random-source-integers-maker s)
  (:random-source-integers-maker s))

(define (random-source-reals-maker s)
  (:random-source-reals-maker s))

(define (random-source-bytevectors-maker s)
  (:random-source-bytevectors-maker s))


;;; MRG32k3a pseudo-random numbers generator

(define (make-random-source)
  (let ((state (vector-copy mrg32k3a-initial-state)))
    (:random-source-make
     (lambda () ; state-ref
       (mrg32k3a-state-internal->external state))
     (lambda (new-state) ; state-set!
       (set! state (mrg32k3a-state-external->internal new-state)))
     (lambda (source-of-integers) ; seed!
       (set! state (mrg32k3a-randomize-state state source-of-integers)))
     (lambda (n) ; integers-maker
       (cond ((not (and (integer? n) (exact? n) (positive? n)))
	      (assertion-violation 'integers-maker
		"range upper limit must be an exact positive integer" n))
	     ((<= n mrg32k3a-m1)
	      (mrg32k3a-random-integer state n))
	     (else
	      (mrg32k3a-random-large state n))))
     (lambda ()	; reals-maker
       (mrg32k3a-random-real state))
     (lambda (number-of-32bit-integers) ; bytevectors-maker
       (mrg32k3a-random-bytevector state number-of-32bit-integers)))))

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
  ;;Produce the next random value  from the stream and advance the state
  ;;of the generator.  We interpret the state vector as the matrix:
  ;;
  ;;  -           -
  ;; | x11 x12 x13 |
  ;; | x21 x22 x23 |
  ;;  -           -
  ;;
  ;;Here we do:
  ;;
  ;;  -  -     -                -   -       -
  ;; | a |   | 1403580  -810728 | | x12 x21 |
  ;; |   | = |                  | |         |
  ;; | b |   | 527612   1370589 | | x13 x23 |
  ;;  - -     -                -   -       -
  ;;
  ;; x10 = a mod mrg32k3a-m1
  ;; x20 = b mod mrg32k3a-m2
  ;;
  ;;then we recompose the state vector shifting the rows:
  ;;
  ;;  -           -
  ;; | x10 x11 x12 |
  ;; | x20 x21 x22 |
  ;;  -           -
  ;;
  (let ((x11 (vector-ref state 0))
        (x12 (vector-ref state 1))
        (x13 (vector-ref state 2))
        (x21 (vector-ref state 3))
        (x22 (vector-ref state 4))
        (x23 (vector-ref state 5)))
    (let ((x10 (modulo (- (* 1403580 x12) (* 810728 x13)) mrg32k3a-m1))
          (x20 (modulo (- (* 527612 x21) (* 1370589 x23)) mrg32k3a-m2)))
      (vector-set! state 0 x10)
      (vector-set! state 1 x11)
      (vector-set! state 2 x12)
      (vector-set! state 3 x20)
      (vector-set! state 4 x21)
      (vector-set! state 5 x22)
      (modulo (- x10 x20) mrg32k3a-m1))))

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
;;; reseed

(define (mrg32k3a-randomize-state state source-of-integers)
  ;; G. Marsaglia's simple 16-bit generator with carry
  (let* ((m 65536)
         (x (modulo (source-of-integers) m)))
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

(define (mrg32k3a-random-integer state range) ; rejection method
  (let* ((q  (quotient mrg32k3a-m1 range))
         (qn (* q range)))
    (do ((x (mrg32k3a-random-m1 state) (mrg32k3a-random-m1 state)))
      ((< x qn) (quotient x q)))))

(define (mrg32k3a-random-real state) ; normalization is 1/(m1+1)
  (* 0.0000000002328306549295728 (+ 1.0 (mrg32k3a-random-m1 state))))

(define (mrg32k3a-random-large state n) ; n > m-max
  (define (mrg32k3a-random-power state k) ; n = m-max^k, k >= 1
    (let ((n (mrg32k3a-random-integer state mrg32k3a-m1)))
      (if (= k 1)
	  n
	(+ n (* mrg32k3a-m1 (mrg32k3a-random-power state (- k 1)))))))
  (do ((k 2 (+ k 1))
       (mk (* mrg32k3a-m1 mrg32k3a-m1) (* mk mrg32k3a-m1)))
      ((>= mk n)
       (let* ((mk-by-n (quotient mk n))
	      (a (* mk-by-n n)))
	 (do ((x (mrg32k3a-random-power state k)
		 (mrg32k3a-random-power state k)))
	     ((< x a)
	      (quotient x mk-by-n)))))))

(define (mrg32k3a-random-bytevector state number-of-32bit-integers)
  (let ((bv (make-bytevector number-of-32bit-integers)))
    (do ((i 0 (+ 1 i)))
	((= i number-of-32bit-integers)
	 bv)
      (bytevector-u32-native-set! bv i (mrg32k3a-random-integer state const:2^32-1)))))


;;;; low level API for randomness from devices

(define (%device-read-bytevector! device bv)
  ;;Fill the  bytevector BV  with bytes from  DEVICE.  Open  the device,
  ;;then close it.
  (let* ((number-of-bytes (bytevector-length bv))
	 (port (open-file-input-port device
				     (file-options no-create)
				     (buffer-mode block))))
    (dynamic-wind
	(lambda () #f)
	(lambda () (get-bytevector-n! port bv 0 number-of-bytes))
	(lambda () (close-port port))))
  bv)

(define (%device-read-bytevector device number-of-bytes)
  ;;Return a  newly allocated bytevectors filled with  random bytes from
  ;;DEVICE.
  (%device-read-bytevector! device (make-bytevector number-of-bytes)))

(define (%device-write-bytevector! device bv)
  ;;Write the bytevector BV to DEVICE.  Open the device, then close it.
  (let ((port (open-file-output-port device
				     (file-options no-create)
				     (buffer-mode block))))
    (dynamic-wind
	(lambda () #f)
	(lambda () (put-bytevector port bv))
	(lambda () (close-port port)))))

;;; --------------------------------------------------------------------

(define (random-device-bytevector! bv)
  ;;Fill the bytevector BV with bytes from "/dev/random".
  (%device-read-bytevector! "/dev/random" bv))

(define (random-device-bytevector number-of-bytes)
  ;;Return  a   newly  allocated  bytevector  filled   with  bytes  from
  ;;"/dev/random".
  (%device-read-bytevector "/dev/random" number-of-bytes))

(define (urandom-device-bytevector! bv)
  ;;Fill the bytevector BV with bytes from "/dev/urandom".
  (%device-read-bytevector! "/dev/urandom" bv))

(define (urandom-device-bytevector number-of-bytes)
  ;;Return  a   newly  allocated  bytevector  filled   with  bytes  from
  ;;"/dev/urandom".
  (%device-read-bytevector "/dev/urandom" number-of-bytes))


;;;; randomness source from devices

(define random-device-cache-length
  (make-parameter 4096
    (lambda (obj)
      (if (and (integer? obj) (exact? obj) (positive? obj))
	  obj
	(assertion-violation 'random-device-cache-length
	  "invalid device cache length, expected positive, exact, integer"
	  obj)))))

(define make-random-source/device
  (case-lambda
   (()
    (make-random-source/device "/dev/urandom"))
   ((device)
    (let* ((device device)
	   (cache (make-bytevector (random-device-cache-length)))
	   (next  (bytevector-length cache)))
		;This init value  causes the vector to be  filled at the
		;first invocation.

      (define (next-integer)
	(when (<= (bytevector-length cache) next)
	  (%device-read-bytevector! device cache)
	  (set! next 0))
	(let ((n (bytevector-u32-native-ref cache next)))
	  (set! next (+ 4 next))
	  n))

      (define (make-integer n)
	(if (and (integer? n) (exact? n) (positive? n))
	    (modulo (next-integer) n)
	  (assertion-violation 'integers-maker
	    "range upper limit must be an exact positive integer" n)))

      (define (make-real)
	(* (inexact (make-integer)) const:1/2^32^2))

      (define (random-device-state-set! new-state)
	(unless (pair? new-state)
	  (assertion-violation 'random-device-state-set!
	    "invalid device randomness source state, expected pair" new-state))
	(unless (eq? 'device (car new-state))
	  (assertion-violation 'random-device-state-set!
	    "invalid device randomness source state, expected \"device\" as signature"
	    (car new-state)))
	(let ((data (cdr new-state)))
	  (unless (and (vector? data) (= 3 (vector-length data)))
	    (assertion-violation 'random-device-state-set!
	      "invalid device randomness source state, expected vector of length 3 as data"
	      data))
	  (let ((_device (vector-ref data 0))
		(_next   (vector-ref data 1))
		(_cache  (vector-ref data 2)))
	    (unless (and (string? _device) (file-exists? _device))
	      (assertion-violation 'random-device-state-set!
		"invalid device randomness source state, expected existent file name as device"
		_device))
	    (unless (and (integer? _next) (exact? _next) (positive? _next))
	      (assertion-violation 'random-device-state-set!
		"invalid device randomness source state, expected positive integer as next index"
		_next))
	    (unless (bytevector? _cache)
	      (assertion-violation 'random-device-state-set!
		"invalid device randomness source state, expected bytevector as randomness pool"
		_cache))
	    (set! device _device)
	    (set! next   _next)
	    (set! cache  _cache))))

      (define (device-randomize-state source-of-integers)
	;;Reads random  32bit unsigned integers  from SOURCE-OF-INTEGERS until
	;;it returns  #f; build  a bytevector  with them and  write it  to the
	;;DEVICE.
	(let-values (((count numbers)
		      (do ((numbers '() (cons (source-of-integers) numbers))
			   (count 0 (+ 1 count)))
			  ((not (car numbers))
			   (values count (cdr numbers))))))
	  (let ((bv (make-bytevector (* 4 count))))
	    (do ((i 0 (+ 1 i))
		 (numbers numbers (cdr numbers)))
		((= i count))
	      (bytevector-u32-native-set! bv i (car numbers)))
	    (%device-write-bytevector! device bv))))

      (:random-source-make
       (lambda () ; state-ref
	 (cons 'device (vector device next cache)))
       (lambda (new-state) ; state-set!
	 (random-device-state-set! new-state))
       (lambda (source-of-integers) ; seed!
	 (device-randomize-state source-of-integers))
       (lambda (n) ; integers-maker
	 (make-integer n))
       (lambda () ; reals-maker
	 (next-integer)))))))


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

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
    random-source-maker			random-source?
    random-source-state-ref		random-source-state-set!
    random-source-seed!
    random-source-integers-maker	random-source-reals-maker
    random-source-bytevectors-maker

    ;; random source constructors
    make-random-source/mrg32k3a		make-random-source/device

    ;; low level API for device-based random sources of numbers
    random-device-cache-length
    %device-read-bytevector!		%device-read-bytevector
    %device-write-bytevector!
    random-device-bytevector!		random-device-bytevector
    urandom-device-bytevector!		urandom-device-bytevector

    ;; no fuss API
    random-integer			random-real
    default-random-source

    ;; utilities
    unfold-random-numbers
    unfold-random-numbers/vector	unfold-random-numbers/string
    random-source-make-permutations	random-source-make-exponentials
    random-source-make-normals)
  (import (rnrs)
    (nausicaa parameters)
    (rnrs mutable-strings))


;;;; helpers

(define const:2^32	(expt 2 32))
(define const:2^32-1	(- const:2^32 1))
(define const:2^32^2	(* const:2^32 const:2^32))
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
	  (immutable required-seed-values)
	  (immutable integers-maker)
	  (immutable reals-maker)
	  (immutable bytevectors-maker)))

(define (random-source-state-ref s)
  ((:random-source-state-ref s)))

(define (random-source-state-set! s state)
  ((:random-source-state-set! s) state))

(define (random-source-seed! s integers-maker)
  ((:random-source-seed! s) integers-maker))

(define (random-source-required-seed-values s)
  (:random-source-required-seed-values s))

(define (random-source-integers-maker s)
  (:random-source-integers-maker s))

(define (random-source-reals-maker s)
  (:random-source-reals-maker s))

(define (random-source-bytevectors-maker s)
  (:random-source-bytevectors-maker s))


;;; MRG32k3a pseudo-random numbers generator

(define (make-random-source/mrg32k3a)
  (let ((A1 1062452522) (A2 2961816100) (A3 342112271)
	(B1 2854655037)	(B2 3321940838)	(B3 3542344109))

    (define external-state-tag 'random-source-state/mrg32k3a)
    (define M1 4294967087) ; modulus of component 1
    (define M2 4294944443) ; modulus of component 2

    (define (compute-random-bits/advance-state)
      (let ((A0 (mod (- (* 1403580 A2) (*  810728 A3)) M1))
	    (B0 (mod (- (*  527612 B1) (* 1370589 B3)) M2)))
	(set! A3 A2) ; shift the A vector right, purging the old A3
	(set! A2 A1)
	(set! A1 A0)
	(set! B3 B2) ; shift the B vector right, purging the old B3
	(set! B2 B1)
	(set! B1 B0)
	(mod (- A0 B0) M1)))

    (define (internal-state->external-state)
      ;;Package the state to be written in a way that can be read back.
      (vector external-state-tag A1 A2 A3 B1 B2 B3))

    (define (external-state->internal-state external-state)
      ;;Given a packaged state, verifies it and return an internal state.
      (define (check-value idx M)
	(let ((S (vector-ref external-state idx)))
	  (if (and (integer? S) (exact? S) (<= 0 S (- M 1)))
	      S
	    (assertion-violation 'external-state->internal-state
	      "illegal random source MRG32k3a state value" S external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 7 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((s1 (check-value 1 M1))
	    (s2 (check-value 2 M1))
	    (s3 (check-value 3 M1))
	    (r1 (check-value 4 M2))
	    (r2 (check-value 5 M2))
	    (r3 (check-value 6 M2)))
	(when (or (zero? (+ s1 s2 s3))
		  (zero? (+ r1 r2 r3)))
	  (assertion-violation 'external-state->internal-state
	    "illegal random source MRG32k3a degenerate state" external-state))
	(set! A1 s1) (set! A2 s2) (set! A3 s3)
	(set! B1 r1) (set! B2 r2) (set! B3 r3)))

    (define (seed! integers-maker)
      ;; G. Marsaglia's simple 16-bit generator with carry
      (let* ((m 65536)
	     (x (mod (integers-maker) m)))
	(define (random n) ; m < n < m^2
	  (define (random-m)
	    (let ((y (mod x m)))
	      (set! x (+ (* 30903 y) (div x m)))
	      y))
	  (mod (+ (* (random-m) m) (random-m)) n))
	(let ((M1-1 (- M1 1))
	      (M2-1 (- M2 1)))
	  (set! A1 (+ 1 (mod (+ A1 (random M1-1)) M1-1)))
	  (set! B1 (+ 1 (mod (+ B1 (random M2-1)) M2-1)))
	  (set! A2 (mod (+ A2 (random M1)) M1))
	  (set! A3 (mod (+ A3 (random M1)) M1))
	  (set! B2 (mod (+ B2 (random M2)) M2))
	  (set! B3 (mod (+ B3 (random M2)) M2)))))

    (define (make-random-integer U)
      (cond ((not (and (integer? U) (exact? U) (positive? U)))
	     (assertion-violation 'make-random-integer
	       "range upper limit must be an exact positive integer" U))
	    ((<= U M1)
	     (make-random-integer/small U))
	    (else
	     (make-random-integer/large U))))

    (define (make-random-integer/small U)
      ;;Read the documentation of Nausicaa/Scheme, node "random prng" to
      ;;understand what this does.
      (let* ((Q  (div M1 U))
	     (QU (* Q U)))
	(do ((N (compute-random-bits/advance-state)
		(compute-random-bits/advance-state)))
	    ((< N QU)
	     (div N Q)))))

    (define (make-random-integer/large U)
      ;;Read the documentation of Nausicaa/Scheme, node "random prng" to
      ;;understand what this does.
      (define (random-polynomial k)
	;;This literally computes:
	;;
	;;   N0 + M1 * (N1 + M1 * (N2^2 + M1 * (N3^3 + ... + M1 * (Nk-1)^(k-1))))
	;;
	;;which can be rewritten:
	;;
	;;   N0 + M1 * N1 + (M1 * N2)^2 + (M1 * N3)^3 + ... + (M1 * N(k-1))^(k-1)
	;;
	(let ((N (make-random-integer/small M1)))
	  (if (= k 1)
	      N
	    (+ N (* M1 (random-polynomial (- k 1)))))))
      (do ((k 2 (+ k 1))
	   (M1^k (* M1 M1) (* M1^k M1)))
	  ((<= U M1^k)
	   (let* ((Q  (div M1^k U))
		  (QU (* Q U)))
	     (do ((N' (random-polynomial k) (random-polynomial k)))
		 ((< N' a)
		  (div N' Q)))))))

    (define (make-random-real)
      ;;Knowing that  the generated integers N  are uniformly distibuted
      ;;in the range 0  <= N < M, a pseudo--random real  number X in the
      ;;range 0  < X <  1 can  be computed from  a generated N  with the
      ;;following normalisation formula:
      ;;
      ;;   X = (1 + N) / (1 + M)
      ;;
      (* 0.0000000002328306549295728 ; = 1 / (1 + M)
	 (+ 1.0 (compute-random-bits/advance-state))))

    (define (make-random-bytevector number-of-32bit-integers)
      (let ((bv (make-bytevector (* 4 number-of-32bit-integers))))
	(do ((i 0 (+ 1 i)))
	    ((= i number-of-32bit-integers)
	     bv)
	  (bytevector-u32-native-set! bv i (make-random-integer/small const:2^32-1)))))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     1				    ; required seed values
     make-random-integer	    ; integers-maker
     make-random-real		    ; reals-maker
     make-random-bytevector)))	    ; bytevectors-maker


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
	    (mod (next-integer) n)
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

      (define (device-randomize-state integers-maker)
	;;Reads random  32bit unsigned integers  from INTEGERS-MAKER until
	;;it returns  #f; build  a bytevector  with them and  write it  to the
	;;DEVICE.
	(let-values (((count numbers)
		      (do ((numbers '() (cons (integers-maker) numbers))
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
       (lambda (integers-maker) ; seed!
	 (device-randomize-state integers-maker))
       #f	   ; required seed values
       (lambda (n) ; integers-maker
	 (make-integer n))
       (lambda () ; reals-maker
	 (next-integer)))))))


;;; no fuss API

(define random-source-maker
  (make-parameter make-random-source/mrg32k3a
    (lambda (obj)
      (if (procedure? obj)
	  obj
	(assertion-violation 'random-source-maker
	  "expected procedure as random source maker" obj)))))

(define default-random-source
  (make-random-source/mrg32k3a))

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

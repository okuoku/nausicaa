;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: random number generator
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;	This is  a combined  multiple recursive generator.  The sequence
;;;	is,
;;;
;;;	   z_n = (x_n - y_n) mod m1
;;;
;;;	where the two underlying generators x and y are,
;;;
;;;	   x_n = (a_{1} x_{n-1} + a_{2} x_{n-2} + a_{3} x_{n-3}) mod m1
;;;	   y_n = (b_{1} y_{n-1} + b_{2} y_{n-2} + b_{3} y_{n-3}) mod m2
;;;
;;;	with coefficients a11 ... a23,
;;;
;;;		a_{1} = 0,     a_{2} = 63308, a_{3} = -183326
;;;		b_{1} = 86098, b_{2} = 0,     b_{3} = -539608
;;;
;;;	and moduli m1, m2,
;;;
;;;		m1 = 2^31 - 1 = 2147483647
;;;		m2 = 2^31 - 2000169 = 2145483479
;;;
;;;	  We initialize the generator with
;;;
;;;	   x_1 = s_1 MOD m1, x_2 = s_2 MOD m1, x_3 = s_3 MOD m1
;;;	   y_1 = s_4 MOD m2, y_2 = s_5 MOD m2, y_3 = s_6 MOD m2
;;;
;;;	where  s_n =  (69069 *  s_{n-1}) mod  2^32 and  s_0 =  s  is the
;;;     user-supplied seed.
;;;
;;;     NOTE: According to the paper the initial values for x_n must lie
;;;     in the range 0 <= x_n <= (m1 - 1) and the initial values for y_n
;;;     must lie in  the range 0 <= y_n  <= (m2 - 1), with  at least one
;;;     non-zero  value   --  our  seeding   procedure  satisfies  these
;;;     constraints.
;;;
;;;     We  then use  7 iterations  of the  generator to  "warm  up" the
;;;     internal state.
;;;
;;;     The theoretical  value of z_{10008} is  719452880. The subscript
;;;     10008 means  (1) seed the generator  with s=1, (2)  do the seven
;;;     warm-up  iterations that are  part of  the seeding  process, (3)
;;;     then do 10000 actual iterations.
;;;
;;;     The period of this generator is about 2^205.
;;;
;;;     From: P.   L'Ecuyer, "Combined Multiple  Recursive Random Number
;;;     Generators," Operations Research, 44, 5 (1996), 816--822.
;;;
;;;     This is available on the net from L'Ecuyer's home page,
;;;
;;;       <http://www.iro.umontreal.ca/~lecuyer/myftp/papers/combmrg.ps>
;;;     <ftp://ftp.iro.umontreal.ca/pub/simulation/lecuyer/papers/combmrg.ps>
;;;
;;;	The Scheme code  was converted by Marco Maggi  from a C language
;;;	version from the GNU Scientific Library release 1.12.
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
;;;Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007 James Theiler, Brian
;;;Gough
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


(library (random cmrg)
  (export make-random-source/cmrg)
  (import (rnrs)
    (random)
    (random low))


(define (make-random-source/cmrg)
  (let ((X1 #f) (X2 #f) (X3 #f)
	(Y1 #f) (Y2 #f) (Y3 #f))

    (define external-state-tag 'random-source-state/cmrg)
    (define M1 2147483647) ; M1 = 2^31 - 1
    (define M2 2145483479) ; M2 = 2^31 - 2000169
    (define const:2^32 (expt 2 32))

    (define A2	  63308) (define QA2 33921) (define RA2 12979)
    (define A3  -183326) (define QA3 11714) (define RA3  2883)
    (define B1    86098) (define QB1 24919) (define RB1  7417)
    (define B3  -539608) (define QB3  3976) (define RB3  2071)

    (define (make-random-bits)
      (define (check-value S M)
	(unless (and (integer? S) (exact? S) (<= 0 S (- M 1)))
	  (assertion-violation 'make-random-bits
	    "illegal random source CMRG state value" S)))
      (check-value X1 M1) (check-value X2 M1) (check-value X3 M1)
      (check-value Y1 M2) (check-value Y2 M2) (check-value Y3 M2)
      ;; component 1
      (let* ((H3 (/ X3 QA3))
	     (P3 (- (* (- A3) (- X3 (* H3 QA3))) (* H3 RA3)))
		;original: p3 = -a3 * (x3 - h3 * qa3) - h3 * ra3
	     (H2 (/ X2 QA2))
	     (P2 (- (*    A2  (- X2 (* H2 QA2))) (* H2 RA2))))
		;originsl: p2 = a2 * (x2 - h2 * qa2) - h2 * ra2
	(when (negative? P3)
	  (set! P3 (+ P3 M1)))
	(when (negative? P2)
	  (set! P2 (+ P2 M1)))
	;;Right-shift the vector X, purging X3.
	(set! X3 X2)
	(set! X2 X1)
	(set! X1 (- P2 P3))
	(when (negative? X1)
	  (set! X1 (+ X1 M1))))
      ;; component 2
      (let* ((H3 (/ Y3 QB3))
	     (P3 (- (* (- B3) (- Y3 (* H3 QB3))) (* H3 RB3)))
		;original: p3 = -b3 * (y3 - h3 * qb3) - h3 * rb3

	     (H1 (/ Y1 QB1))
	     (P1 (- (* B1 (- Y1 (* H1 QB1))) (* H1 RB1))))
		;original: p1 = b1 * (y1 - h1 * qb1) - h1 * rb1
	(when (negative? P3)
	  (set! P3 (+ P3 M2)))
	(when (negative? P1)
	  (set! P1 (+ P1 M2)))
	;;Right-shift the vector Y, purging Y3.
	(set! Y3 Y2)
	(set! Y2 Y1)
	(set! Y1 (- P1 P3))
	(when (negative? Y1)
	  (set! Y1 (+ Y1 M2))))
      (let ((N (- X1 Y1)))
	(if (< X1 Y1)
	    (+ N M1)
	  N)))

    (define (make-random-32bits)
      (make-random-integer M1 M1 make-random-bits))

    (define (seed! integers-maker)
      (%seed! (integers-maker)))

    (define (%seed! S)
      ;;An  entirely adhoc  way  of  seeding! This  does  NOT come  from
      ;;L'Ecuyer et al.
      (define (LCG n)
	(mod (* 69069 n) const:2^32))
      (write (list 'seed S))(newline)
      (set! S  (LCG S))
      (set! X1 (mod S M1))
      (write (list 'seed X1 X2 X3 Y1 Y2 Y3))(newline)
      (set! S  (LCG S))
      (set! X2 (mod S M1))
      (set! S  (LCG S))
      (set! X3 (mod S M1))

      (set! S  (LCG S))
      (set! Y1 (mod S M2))
      (set! S  (LCG S))
      (set! Y2 (mod S M2))
      (set! S  (LCG S))
      (set! Y3 (mod S M2))
      (write (list 'seed X1 X2 X3 Y1 Y2 Y3))(newline)
      (do ((i 0 (+ 1 i)))
	  ((= i 7))
	(make-random-bits)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag X1 X2 X3 Y1 Y2 Y3))

    (define (external-state->internal-state external-state)
      (define (check-value idx M)
	(let ((S (vector-ref external-state idx)))
	  (if (and (integer? S) (exact? S) (<= 0 S (- M 1)))
	      S
	    (assertion-violation 'external-state->internal-state
	      "illegal random source CMRG state value" S external-state))))
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
	    "illegal random source CMRG degenerate state" external-state))
	(set! X1 s1) (set! X2 s2) (set! X3 s3)
	(set! Y1 r1) (set! Y2 r2) (set! Y3 r3)))

    ;;Initialisation.
    (write 'here)(newline)
    (%seed! 1)
    (write (list 'here X1 X2 X3 Y1 Y2 Y3))(newline)
    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     1				    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U M1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real M1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real M1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


;;;; done

)

;;; end of file

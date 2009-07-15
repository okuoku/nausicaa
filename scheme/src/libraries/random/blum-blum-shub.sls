;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: BBS random number generator
;;;Date: Sat Jul  4, 2009
;;;
;;;Abstract
;;;
;;;	The  Blum-Blum-Shub  is a  cryptographically  secure PRNG.   The
;;;	implementation is derived from:
;;;
;;;		<http://it.wikipedia.org/wiki/Blum_Blum_Shub>
;;;		<http://en.wikipedia.org/wiki/Blum_Blum_Shub>
;;;
;;;
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


#!r6rs
(library (random blum-blum-shub)
  (export make-random-source/blum-blum-shub)
  (import (rnrs)
    (random)
    (random low))


(define (make-random-source/blum-blum-shub)
  (let* ((P #f) (Q #f) (PQ #f) (X #f))

    (define c 0)
    (define M const:2^32)

    (define (%init P? Q? integer-maker)
      (let* ((PQ?  (* P? Q?))
	     (PQ-1 (- PQ? 1))
	     (S   (do ((S (integer-maker PQ-1) (integer-maker PQ-1)))
		      ((= 1 (gcd S PQ?))
		       S))))
	(set! P P?)
	(set! Q Q?)
	(set! PQ PQ?)
	(set! X (mod (* S S) PQ))))

    (define external-state-tag 'random-source-state/blum-blum-shub)

    (define (make-random-bits)
      (set! c (+ 1 c))
      (let* ((N 0)
	     (Xn (do ((i 0 (+ 1 i))
		      (Xn X (mod (* Xn Xn) PQ)))
		     ((= i 32)
		      Xn)
		   (set! N (bitwise-copy-bit N i (mod (bitwise-bit-count Xn) 2))))))
	(set! X Xn)
	N))

    (define make-random-32bits make-random-bits)

    (define (seed! integers-maker)
      (let ((P (integers-maker const:2^32))
	    (Q (integers-maker const:2^32)))
	(unless (= 3 (mod P 4))
	  (assertion-violation 'seed!
	    "expected prime number P congruent to 3 (mod 4)" P))
	(unless (= 3 (mod Q 4))
	  (assertion-violation 'seed!
	    "expected prime number Q congruent to 3 (mod 4)" Q))
	(%init P Q integers-maker)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag P Q PQ X))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source FIB state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 5 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((P0  (check-it 1))
	    (Q0  (check-it 2))
	    (PQ0 (check-it 3))
	    (X0  (check-it 4)))
	(set! P  P0)
	(set! Q  Q0)
	(set! PQ PQ0)
	(set! X  X0)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     +inf.0			    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U M make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real M make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real M make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


;;;; done

)

;;; end of file

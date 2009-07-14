;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: George Marsaglia's PRNGs
;;;Date: Tue Jul 14, 2009
;;;
;;;Abstract
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
(library (random marsaglia)
  (export
    make-random-source/marsaglia/cong
    make-random-source/marsaglia/fib
    make-random-source/marsaglia/lfib4
    make-random-source/marsaglia/kiss
    make-random-source/marsaglia/mwc
    make-random-source/marsaglia/shr3
    make-random-source/marsaglia/swb)
  (import (rnrs)
    (random)
    (random low))


;;;; helpers

(define const:2^32	(expt 2 32))
(define const:2^32-1	(- const:2^32 1))

(define ^		bitwise-xor)
(define &		bitwise-and)

(define-syntax ADD
  (syntax-rules ()
    ((_ ?arg ...)
     (mod (+ ?arg ...) const:2^32))))

(define-syntax ADD256
  (syntax-rules ()
    ((_ ?arg ...)
     (mod (+ ?arg ...) 256))))

(define-syntax SUB
  (syntax-rules ()
    ((_ ?arg ...)
     (mod (- ?arg ...) const:2^32))))

(define-syntax MUL
  (syntax-rules ()
    ((_ ?arg ...)
     (mod (* ?arg ...) const:2^32))))

(define-syntax >>
  (syntax-rules ()
    ((_ ?integer ?amount)
     (mod (bitwise-arithmetic-shift-right ?integer ?amount) const:2^32))))

(define-syntax <<
  (syntax-rules ()
    ((_ ?integer ?amount)
     (mod (bitwise-arithmetic-shift-left ?integer ?amount) const:2^32))))

(define-syntax u32-set!
  (syntax-rules ()
    ((_ ?vector ?index ?value)
     (bytevector-u32-native-set! ?vector (* 4 ?index) ?value))))

(define-syntax u32-ref
  (syntax-rules ()
    ((_ ?vector ?index)
     (bytevector-u32-native-ref ?vector (* 4 ?index)))))


;;;; generators

;;The arguments to the macros are always in lexicographic (alphabetical)
;;order.

(define-syntax CONG
  ;;Original:
  ;;
  ;;#define CONG	(ctx->jcong = 69069 * ctx->jcong + 1234567)
  ;;
  (syntax-rules ()
    ((_ ?jcong)
     (begin
       (set! ?jcong (ADD (MUL 69069 ?jcong) 1234567))
       ?jcong))))

(define-syntax FIB
  ;;Original:
  ;;
  ;;#define FIB		((ctx->b = ctx->a + ctx->b), (ctx->a = ctx->b - ctx->a))
  ;;
  (syntax-rules ()
    ((_ ?a ?b)
     (begin
       (set! ?b (ADD ?a ?b))
       (set! ?a (SUB ?b ?a))
       ?a))))

(define-syntax LFIB4
  ;;Original:
  ;;
  ;;#define LFIB4	((ctx->c)++,
  ;;			 ctx->t[ctx->c] = ctx->t[ctx->c]
  ;;					+ ctx->t[ctx->c + 58]
  ;;			                + ctx->t[ctx->c + 119]
  ;;			                + ctx->t[ctx->c + 178])
  ;;
  (syntax-rules ()
    ((_ ?c ?t)
     (begin
       (u32-set! ?t ?c
		 (ADD (u32-ref ?t ?c)
		      (u32-ref ?t (ADD256 ?c  58))
		      (u32-ref ?t (ADD256 ?c 119))
		      (u32-ref ?t (ADD256 ?c 178))))
       (let ((v (u32-ref ?t ?c)))
	 (set! ?c (ADD256 1 ?c))
	 v)))))

(define-syntax KISS
  ;;Original:
  ;;
  ;;#define KISS	((MWC^CONG) + SHR3)
  ;;
  (syntax-rules ()
    ((_ ?jcong ?jsr ?w ?z)
     (ADD (^ (MWC ?w ?z) (CONG ?jcong)) (SHR3 ?jsr)))))

(define-syntax MWC
  ;;Original:
  ;;
  ;;#define MWC	((znew << 16) + wnew)
  ;;
  (syntax-rules ()
    ((_ ?w ?z)
     (ADD (<< (znew ?z) 16) (wnew ?w)))))

(define-syntax SHR3
  ;;Original:
  ;;
  ;;#define SHR3	(ctx->jsr ^= (ctx->jsr << 17),
  ;;			 ctx->jsr ^= (ctx->jsr >> 13),
  ;;			 ctx->jsr ^= (ctx->jsr << 5))
  ;;
  (syntax-rules ()
    ((_ ?jsr)
     (begin
       (set! ?jsr (^ ?jsr (<< ?jsr 17)))
       (set! ?jsr (^ ?jsr (>> ?jsr 13)))
       (set! ?jsr (^ ?jsr (<< ?jsr 5)))
       ?jsr))))

(define-syntax SWB
  ;;Original:
  ;;
  ;;#define SWB		((ctx->c)++,
  ;;			 ctx->bro = (ctx->x < ctx->y),
  ;;			 ctx->t[ctx->c] = (ctx->x = ctx->t[ctx->c + 34])
  ;;			                - (ctx->y = ctx->t[ctx->c + 19]
  ;;			                          + ctx->bro))
  ;;
  (syntax-rules ()
    ((_ ?bro ?c ?t ?x ?y)
     (begin
       (set! ?bro (if (< ?x ?y) 1 0))
       (u32-set! ?t ?c
		 (SUB (begin
			(set! ?x (u32-ref ?t (ADD256 ?c 34)))
			?x)
		      (ADD (begin
			     (set! ?y (u32-ref ?t (ADD256 ?c 19)))
			     ?y)
			   ?bro)))
       (let ((v (u32-ref ?t ?c)))
	 (set! ?c (ADD256 1 ?c))
	 v)))))

;;; --------------------------------------------------------------------
;;; The following are helpers.

(define-syntax wnew
  ;;Original:
  ;;
  ;;#define wnew	(ctx->w = 18000 * (ctx->w & 65535) + (ctx->w >> 16))
  ;;
  (syntax-rules ()
    ((_ ?w)
     (begin
       (set! ?w (ADD (MUL 18000 (& ?w 65535)) (>> ?w 16)))
       ?w))))

(define-syntax znew
  ;;Original:
  ;;
  ;;#define znew	(ctx->z = 36969 * (ctx->z & 65535) + (ctx->z >> 16))
  ;;
  (syntax-rules ()
    ((_ ?z)
     (begin
       (set! ?z (ADD (MUL 36969 (& ?z 65535)) (>> ?z 16)))
       ?z))))


(define (make-random-source/marsaglia/cong)
  (let ((jcong 380116160))

    (define external-state-tag 'random-source-state/marsaglia/cong)

    (define (make-random-bits)
      (CONG jcong))

    (define (make-random-32bits)
      (make-random-integer const:2^32 const:2^32-1 make-random-bits))

    (define (seed! integers-maker)
      (set! jcong (integers-maker const:2^32)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag jcong))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 2 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((Y (vector-ref external-state 1)))
	(if (and (integer? Y) (exact? Y) (<= 0 Y) (<= Y const:2^32-1))
	    (set! jcong Y)
	  (assertion-violation 'external-state->internal-state
	    "illegal random source CONG state" external-state))))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     1				    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U const:2^32-1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


(define (make-random-source/marsaglia/fib)
  (let ((A 224466889)
	(B 7584631))

    (define external-state-tag 'random-source-state/marsaglia/fib)

    (define (make-random-bits)
      (FIB A B))

    (define (make-random-32bits)
      (make-random-integer const:2^32 const:2^32-1 make-random-bits))

    (define (seed! integers-maker)
      (set! A (integers-maker const:2^32))
      (set! B (integers-maker const:2^32)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag A B))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y) (<= Y const:2^32-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source FIB state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 3 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((X (check-it 1))
	    (Y (check-it 2)))
	(set! A X)
	(set! B Y)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     2				    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U const:2^32-1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


(define (make-random-source/marsaglia/lfib4)
  (let* ((buffer-length/32bits 256)
	 (buffer-length/bytes  (* 4 buffer-length/32bits))
	 (t (make-bytevector buffer-length/bytes))
	 (c 0))

    (define external-state-tag 'random-source-state/marsaglia/lfib4)

    (define (make-random-bits)
      (LFIB4 c t))

    (define (make-random-32bits)
      (make-random-integer const:2^32 const:2^32-1 make-random-bits))

    (define (seed! integers-maker)
      (set! c 0)
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(bytevector-u32-native-set! t (* 4 i) (integers-maker const:2^32))))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (let ((state (make-vector (+ buffer-length/32bits 1 1))))
	(vector-set! state 0 external-state-tag)
	(vector-set! state 1 c)
	(do ((i 0 (+ 1 i)))
	    ((= i buffer-length/32bits)
	     state)
	  (vector-set! state (+ 2 i) (bytevector-u32-native-ref t (* 4 i))))))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y) (<= Y const:2^32-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source LFIB4 state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= (+ buffer-length/32bits 1 1) (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(check-it (+ 2 i)))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(bytevector-u32-native-set! t (* 4 i) (vector-ref external-state (+ 2 i)))))

    ;;Initial seed.
    (let ((source (make-random-source/marsaglia/kiss))
	  (sub-seeds (let ((ell '(12345	   ;; jcong
				  34221	   ;; jsr
				  65435	   ;; w
				  12345))) ;; z
		       (lambda (U)
			 (let ((N (car ell)))
			   (set! ell (cdr ell))
			   N)))))
      (random-source-seed! source sub-seeds)
      (seed! (random-source-integers-maker source)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     buffer-length/32bits	    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U const:2^32-1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


(define (make-random-source/marsaglia/kiss)
  (let ((jcong 380116160)
	(jsr   123456789)
	(w     521288629)
	(z     362436069))

    (define external-state-tag 'random-source-state/marsaglia/kiss)

    (define (make-random-bits)
      (KISS jcong jsr w z))

    (define (make-random-32bits)
      (make-random-integer const:2^32 const:2^32-1 make-random-bits))

    (define (seed! integers-maker)
      (set! jcong (integers-maker const:2^32))
      (set! jsr   (integers-maker const:2^32))
      (set! w     (integers-maker const:2^32))
      (set! z     (integers-maker const:2^32)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag jcong jsr w z))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y) (<= Y const:2^32-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source KISS state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 5 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((Y0 (check-it 1))
	    (Y1 (check-it 2))
	    (Y2 (check-it 3))
	    (Y3 (check-it 4)))
	(set! jcong Y0)
	(set! jsr   Y1)
	(set! w     Y2)
	(set! z     Y3)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     4				    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U const:2^32-1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


(define (make-random-source/marsaglia/mwc)
  (let ((W 224466889)
	(Z 7584631))

    (define external-state-tag 'random-source-state/marsaglia/mwc)

    (define (make-random-bits)
      (MWC W Z))

    (define (make-random-32bits)
      (make-random-integer const:2^32 const:2^32-1 make-random-bits))

    (define (seed! integers-maker)
      (set! W (integers-maker const:2^32))
      (set! Z (integers-maker const:2^32)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag W Z))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y) (<= Y const:2^32-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source MWC state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 3 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (let ((X0 (check-it 1))
	    (X1 (check-it 2)))
	(set! W X0)
	(set! Z X1)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     2				    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U const:2^32-1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


(define (make-random-source/marsaglia/shr3)
  (let ((jsr 123456789))

    (define external-state-tag 'random-source-state/marsaglia/shr3)

    (define (make-random-bits)
      (SHR3 jsr))

    (define (make-random-32bits)
      (make-random-integer const:2^32 const:2^32-1 make-random-bits))

    (define (seed! integers-maker)
      (set! jsr (integers-maker const:2^32)))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (vector external-state-tag jsr))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y) (<= Y const:2^32-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source SHR3 state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= 2 (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (set! jsr (check-it 1)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     1				    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U const:2^32-1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


(define (make-random-source/marsaglia/swb)
  (let* ((buffer-length/32bits 256)
	 (buffer-length/bytes  (* 4 buffer-length/32bits))
	 (t   (make-bytevector buffer-length/bytes))
	 (bro 0)
	 (c   0)
	 (x   0)
	 (y   0))

    (define external-state-tag 'random-source-state/marsaglia/swb)

    (define (make-random-bits)
      (SWB bro c t x y))

    (define (make-random-32bits)
      (make-random-integer const:2^32 const:2^32-1 make-random-bits))

    (define (seed! integers-maker)
      (set! c 0) (set! x 0) (set! y 0) (set! bro 0)
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(bytevector-u32-native-set! t (* 4 i) (integers-maker const:2^32))))

    (define (jumpahead! number-of-steps)
      (do ((i 0 (+ 1 i)))
	  ((= i number-of-steps))
	(make-random-bits)))

    (define (internal-state->external-state)
      (let ((state (make-vector (+ buffer-length/32bits 1 4))))
	(vector-set! state 0 external-state-tag)
	(vector-set! state 1 bro)
	(vector-set! state 2 c)
	(vector-set! state 3 x)
	(vector-set! state 4 y)
	(do ((i 0 (+ 1 i)))
	    ((= i buffer-length/32bits)
	     state)
	  (vector-set! state (+ 5 i) (bytevector-u32-native-ref t (* 4 i))))))

    (define (external-state->internal-state external-state)
      ;;If the external state is  invalid, the randomness source must be
      ;;left in the previous, correct state.
      (define (check-it idx)
	(let ((Y (vector-ref external-state idx)))
	  (if (and (integer? Y) (exact? Y) (<= 0 Y) (<= Y const:2^32-1))
	      Y
	    (assertion-violation 'external-state->internal-state
	      "illegal random source SWB state" Y external-state))))
      (unless (and (vector? external-state)
		   (eq? external-state-tag (vector-ref external-state 0))
		   (= (+ buffer-length/32bits 1 4) (vector-length external-state)))
	(assertion-violation 'external-state->internal-state
	  "invalid external state argument" external-state))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(check-it (+ 5 i)))
      (let ((X0 (check-it 1))
	    (X1 (check-it 2))
	    (X2 (check-it 3))
	    (X3 (check-it 4)))
	(set! bro X0)
	(set! c   X1)
	(set! x   X2)
	(set! y   X3))
      (do ((i 0 (+ 1 i)))
	  ((= i buffer-length/32bits))
	(bytevector-u32-native-set! t (* 4 i) (vector-ref external-state (+ 5 i)))))

    ;;Initial seed.
    (let ((source (make-random-source/marsaglia/kiss))
	  (sub-seeds (let ((ell '(12345	   ;; jcong
				  34221	   ;; jsr
				  65435	   ;; w
				  12345))) ;; z
		       (lambda (U)
			 (let ((N (car ell)))
			   (set! ell (cdr ell))
			   N)))))
      (random-source-seed! source sub-seeds)
      (seed! (random-source-integers-maker source)))

    (:random-source-make
     internal-state->external-state ; state-ref
     external-state->internal-state ; state-set!
     seed!			    ; seed!
     jumpahead!			    ; jumpahead!
     buffer-length/32bits	    ; required seed values
     (lambda (U)		    ; integers-maker
       (make-random-integer U const:2^32-1 make-random-bits))
     (case-lambda ; reals-maker
      (()
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits)))
      ((unit)
       (lambda ()
	 (make-random-real const:2^32-1 make-random-bits unit))))
     (lambda (bv) ; bytevectors-filler
       (random-bytevector-fill! bv make-random-32bits)))))


;;;; done

)

;;; end of file

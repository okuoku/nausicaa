;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level memory functions
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (nausicaa)
  (checks)
  (ffi memory)
  (ffi memory compensated)
  (ffi sizeof)
  (formations)
  (compensations))

(check-set-mode! 'report-failed)
(display "*** testing memory\n")

(cond-expand ((or larceny petite) (exit)) (else #f))


(parameterize ((check-test-name 'pokers))

  (define-syntax doit
    (syntax-rules ()
      ((_ ?value ?type)
       (let* ((p #f))
	 (dynamic-wind
	     (lambda () (set! p (malloc (expt 10 5))))
	     (lambda ()
;;;	       (write (list 'poking ?value))(newline)
	       (pointer-c-set! ?type p 100 ?value)
	       (let ((v (pointer-c-ref ?type p 100)))
;;;		 (write (list 'peeked v))(newline)
		 v))
	     (lambda () (primitive-free p)))))))
  (define-syntax generic-test-it
    (syntax-rules ()
      ((_ ?value ?type)
       (check (doit ?value ?type) => ?value))))
  (define-syntax generic-test-it/error
    (syntax-rules ()
      ((_ ?value ?type)
       (check (guard (exc ((condition? exc) #t)
			  (else #f))
		(doit ?value ?type))
	 => #t))))

;;; --------------------------------------------------------------------
;;; char

  (let ()
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value signed-char))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value signed-char))))
    (test-it 65)
    (test-it 0)
    (test-it 127)
    (test-it -128)
    (test-it/error 200)
    (test-it/error -200))

  (let ()
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value unsigned-char))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value unsigned-char))))
    (test-it 65)
    (test-it 0)
    (test-it 255)
    (test-it/error 300)
    (test-it/error -200))

;;; --------------------------------------------------------------------
;;; short

  (let* ((bits	16)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value signed-short))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value signed-short))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

  (let* ((bits	16)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value unsigned-short))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value unsigned-short))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------
;;; int

  (let* ((bits	32)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value signed-int))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value signed-int))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 101))
    (test-it/error (- min 100)))

  (let* ((bits	32)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value unsigned-int))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value unsigned-int))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------
;;; long

  (let* ((bits	(if (c-inspect on-32-bits-system) 32 64))
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value signed-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value signed-long))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

  (let* ((bits	(if (c-inspect on-32-bits-system) 32 64))
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value unsigned-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value unsigned-long))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

;;; --------------------------------------------------------------------
;;; long-long

  (let* ((bits	64)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value signed-long-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value signed-long-long))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

  (let* ((bits	64)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value unsigned-long-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value unsigned-long-long))))
    (test-it 0)
    (test-it 65)
    (test-it 66)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

;;; --------------------------------------------------------------------
;;; int8_t

  (let ()
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value int8_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value int8_t))))
    (test-it 65)
    (test-it 0)
    (test-it 127)
    (test-it -128)
    (test-it/error 200)
    (test-it/error -200))

  (let ()
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value uint8_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value uint8_t))))
    (test-it 65)
    (test-it 0)
    (test-it 255)
    (test-it/error 300)
    (test-it/error -200))

;;; --------------------------------------------------------------------
;;; int16

  (let* ((bits	16)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value int16_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value int16_t))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

  (let* ((bits	16)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value uint16_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value uint16_t))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------
;;; int32

  (let* ((bits	32)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value int32_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value int32_t))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 101))
    (test-it/error (- min 100)))

  (let* ((bits	32)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value uint32_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value uint32_t))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------
;;; int64

  (let* ((bits	64)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value int64_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value int64_t))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

  (let* ((bits	64)
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value uint64_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value uint64_t))))
    (test-it 0)
    (test-it 65)
    (test-it 66)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

;;; --------------------------------------------------------------------
;;; float, double

  (check
      (let* ((p (malloc (expt 10 5)))
	     (d (begin
		  (pointer-c-set! float p 64 4.5)
		  (pointer-c-ref  float p 64))))
	(primitive-free p)
	d)
    => 4.5)

  (check
      (let* ((p (malloc (expt 10 5)))
	     (d (begin
		  (pointer-c-set! double p 64 4.5)
		  (pointer-c-ref  double p 64))))
	(primitive-free p)
	d)
    => 4.5)

;;; --------------------------------------------------------------------
;;; size_t, ssize_t

  (let* ((max	(c-valueof ssize_t-max))
	 (min	(c-valueof ssize_t-min)))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value ssize_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value ssize_t))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 101))
    (test-it/error (- min 100)))

  (let* ((max	(c-valueof size_t-max))
	 (min	(c-valueof size_t-min)))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value size_t))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value size_t))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------
;;; pointer

  (check
      (let* ((p (malloc (expt 10 5))))
	(begin0
	    (begin
	      (pointer-c-set! pointer p 100 (integer->pointer 90))
	      (pointer->integer (pointer-c-ref pointer p 100)))
	  (primitive-free p)))
    => 90)

  #t)


(parameterize ((check-test-name 'array))

  (check
      (with-compensations
	(let ((a (malloc/c (c-sizeof char 16))))
	  (array-c-set! signed-char a 5 65)
	  (array-c-set! unsigned-char a 6 66)
	  (list (array-c-ref signed-char a 5)
		(array-c-ref unsigned-char a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (c-sizeof int 16))))
	  (array-c-set! signed-int a 5 65)
	  (array-c-set! unsigned-int a 6 66)
	  (list (array-c-ref signed-int a 5)
		(array-c-ref unsigned-int a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (c-sizeof short 16))))
	  (array-c-set! signed-short a 5 65)
	  (array-c-set! unsigned-short a 6 66)
	  (list (array-c-ref signed-short a 5)
		(array-c-ref unsigned-short a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (c-sizeof long 16))))
	  (array-c-set! signed-long a 5 65)
	  (array-c-set! unsigned-long a 6 66)
	  (list (array-c-ref signed-long a 5)
		(array-c-ref unsigned-long a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (c-sizeof long-long 16))))
	  (array-c-set! signed-long-long a 5 65)
	  (array-c-set! unsigned-long-long a 6 66)
	  (list (array-c-ref signed-long-long a 5)
		(array-c-ref unsigned-long-long a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (c-sizeof float 16))))
	  (array-c-set! float a 5 65.1)
	  (array-c-set! float a 6 66.2)
	  (cons (array-c-ref float a 5)
		(array-c-ref float a 6))))
    (=> (lambda (a b)
	  (and (< (- (car a) (car b)) 0.1)
	       (< (- (cdr a) (cdr b)) 0.1))))
    '(65.1 . 66.2))

  (check
      (with-compensations
	(let ((a (malloc/c (c-sizeof double 16))))
	  (array-c-set! double a 5 65.1)
	  (array-c-set! double a 6 66.2)
	  (cons (array-c-ref double a 5)
		(array-c-ref double a 6))))
    (=> (lambda (a b)
	  (and (< (- (car a) (car b)) 0.1)
	       (< (- (cdr a) (cdr b)) 0.1))))
    '(65.1 . 66.2))

  (check
      (with-compensations
	(let ((a (malloc/c (c-sizeof pointer 16))))
	  (array-c-set! pointer a 5 (integer->pointer 65))
	  (array-c-set! pointer a 6 (integer->pointer 66))
	  (map pointer->integer
	    (list (array-c-ref pointer a 5)
		  (array-c-ref pointer a 6)))))
    => '(65 66))

  #t)


;;;; done

(check-report)

;;; end of file

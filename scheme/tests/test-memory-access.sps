;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level memory functions
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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
  (foreign memory)
  (foreign memory compensated)
  (foreign ffi sizeof)
  (format)
  (compensations))

(check-set-mode! 'report-failed)
(display "*** testing memory\n")


(parameterize ((check-test-name 'pokers))

  (define-syntax doit
    (syntax-rules ()
      ((_ value setter getter)
       (let* ((p #f))
	 (dynamic-wind
	     (lambda () (set! p (malloc (expt 10 5))))
	     (lambda ()
;;;	       (write (list 'poking value))(newline)
	       (setter p 100 value)
	       (let ((v (getter p 100)))
;;;		 (write (list 'peeked v))(newline)
		 v))
	     (lambda () (primitive-free p)))))))
  (define-syntax generic-test-it
    (syntax-rules ()
      ((_ value setter getter)
       (check (doit value setter getter) => value))))
  (define-syntax generic-test-it/error
    (syntax-rules ()
      ((_ value setter getter)
       (check (guard (exc ((condition? exc) #t)
			  (else #f))
		(doit value setter getter))
	 => #t))))

;;; --------------------------------------------------------------------

  (let ()
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-signed-char! pointer-ref-c-signed-char))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-char! pointer-ref-c-signed-char))))
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
	 (generic-test-it value pointer-set-c-unsigned-char! pointer-ref-c-unsigned-char))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-char! pointer-ref-c-unsigned-char))))
    (test-it 65)
    (test-it 0)
    (test-it 255)
    (test-it/error 300)
    (test-it/error -200))

;;; --------------------------------------------------------------------

  (let* ((bits	16)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-signed-short! pointer-ref-c-signed-short))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-short! pointer-ref-c-signed-short))))
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
	 (generic-test-it value pointer-set-c-unsigned-short! pointer-ref-c-unsigned-short))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-short! pointer-ref-c-unsigned-short))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------

  (let* ((bits	32)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1))))
	 (poke pointer-set-c-signed-int!)
	 (peek pointer-ref-c-signed-int))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value poke peek))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-int! pointer-ref-c-signed-int))))
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
	 (generic-test-it value pointer-set-c-unsigned-int! pointer-ref-c-unsigned-int))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-int! pointer-ref-c-unsigned-int))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

;;; --------------------------------------------------------------------

  (let* ((bits	(if on-32-bits-system 32 64))
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-signed-long! pointer-ref-c-signed-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-long! pointer-ref-c-signed-long))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100)))

  (let* ((bits	(if on-32-bits-system 32 64))
	 (max	(- (expt 2 bits) 1))
	 (min	0))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-unsigned-long! pointer-ref-c-unsigned-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-long! pointer-ref-c-unsigned-long))))
    (test-it 0)
    (test-it 65)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

;;; --------------------------------------------------------------------

  (let* ((bits	64)
	 (max	(- (expt 2 (- bits 1)) 1))
	 (min	(- (expt 2 (- bits 1)))))
    (define-syntax test-it
      (syntax-rules ()
	((_ value)
	 (generic-test-it value pointer-set-c-signed-long-long! pointer-ref-c-signed-long-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-signed-long-long!
				pointer-ref-c-signed-long-long))))
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
	 (generic-test-it value pointer-set-c-unsigned-long-long! pointer-ref-c-unsigned-long-long))))
    (define-syntax test-it/error
      (syntax-rules ()
	((_ value)
	 (generic-test-it/error value pointer-set-c-unsigned-long-long!
				pointer-ref-c-unsigned-long-long))))
    (test-it 0)
    (test-it 65)
    (test-it 66)
    (test-it max)
    (test-it min)
    (test-it/error (+ max 100))
    (test-it/error (- min 100))
    )

;;; --------------------------------------------------------------------

  (check
      (let* ((p (malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-float! p 64 4.5)
		  (pointer-ref-c-float p 64))))
	(primitive-free p)
	d)
    => 4.5)

  (check
      (let* ((p (malloc (expt 10 5)))
	     (d (begin
		  (pointer-set-c-double! p 64 4.5)
		  (pointer-ref-c-double p 64))))
	(primitive-free p)
	d)
    => 4.5)

;;; --------------------------------------------------------------------

  (check
      (let* ((p (malloc (expt 10 5))))
	(begin0
	    (begin
	      (pointer-set-c-pointer! p 100 (integer->pointer 90))
	      (pointer->integer (pointer-ref-c-pointer p 100)))
	  (primitive-free p)))
    => 90)

  #t)


(parameterize ((check-test-name 'array))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-char-array 16))))
	  (poke-array-signed-char! a 5 65)
	  (poke-array-unsigned-char! a 6 66)
	  (list (peek-array-signed-char a 5)
		(peek-array-unsigned-char a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-int-array 16))))
	  (poke-array-signed-int! a 5 65)
	  (poke-array-unsigned-int! a 6 66)
	  (list (peek-array-signed-int a 5)
		(peek-array-unsigned-int a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-short-array 16))))
	  (poke-array-signed-short! a 5 65)
	  (poke-array-unsigned-short! a 6 66)
	  (list (peek-array-signed-short a 5)
		(peek-array-unsigned-short a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-long-array 16))))
	  (poke-array-signed-long! a 5 65)
	  (poke-array-unsigned-long! a 6 66)
	  (list (peek-array-signed-long a 5)
		(peek-array-unsigned-long a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-long-long-array 16))))
	  (poke-array-signed-long-long! a 5 65)
	  (poke-array-unsigned-long-long! a 6 66)
	  (list (peek-array-signed-long-long a 5)
		(peek-array-unsigned-long-long a 6))))
    => '(65 66))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-float-array 16))))
	  (poke-array-float! a 5 65.1)
	  (poke-array-float! a 6 66.2)
	  (cons (peek-array-float a 5)
		(peek-array-float a 6))))
    (=> (lambda (a b)
	  (and (< (- (car a) (car b)) 0.1)
	       (< (- (cdr a) (cdr b)) 0.1))))
    '(65.1 . 66.2))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-double-array 16))))
	  (poke-array-double! a 5 65.1)
	  (poke-array-double! a 6 66.2)
	  (cons (peek-array-double a 5)
		(peek-array-double a 6))))
    (=> (lambda (a b)
	  (and (< (- (car a) (car b)) 0.1)
	       (< (- (cdr a) (cdr b)) 0.1))))
    '(65.1 . 66.2))

  (check
      (with-compensations
	(let ((a (malloc/c (sizeof-pointer-array 16))))
	  (poke-array-pointer! a 5 (integer->pointer 65))
	  (poke-array-pointer! a 6 (integer->pointer 66))
	  (map pointer->integer
	    (list (peek-array-pointer a 5)
		  (peek-array-pointer a 6)))))
    => '(65 66))

  #t)


;;;; done

(check-report)

;;; end of file

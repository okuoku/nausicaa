;;;
;;;Part of: Glibc libraries for R6RS Scheme
;;;Contents: extended stream functions
;;;Date: Thu Dec  4, 2008
;;;Time-stamp: <2008-12-04 13:55:02 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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

(library (glibc streams-extended)
  (export
    freadable fwritable freading fwriting fwide
    getline primitive-getline
    getdelim primitive-getdelim
    fpurge)
  (import (rnrs)
    (uriel lang)
    (uriel ffi)
    (srfi receive))

  (define libc
    (begin
      (shared-object self-shared-object)
      self-shared-object))

  (define-c-function primitive-freadable
    (int __freadable (FILE*)))

  (define-c-function primitive-fwritable
    (int __fwritable (FILE*)))

  (define-c-function primitive-freading
    (int __freading (FILE*)))

  (define-c-function primitive-fwriting
    (int __fwriting (FILE*)))

  (define (freadable stream)
    (not (= 0 (primitive-freadable stream))))

  (define (fwritable stream)
    (not (= 0 (primitive-fwritable stream))))

  (define (freading stream)
    (not (= 0 (primitive-freading stream))))

  (define (fwriting stream)
    (not (= 0 (primitive-fwriting stream))))

  (define-c-function fwide
    (int fwide (FILE* int)))

  (define-c-function/with-errno primitive-getline
    (ssize_t getline (void* void* FILE*)))

  (define-c-function/with-errno primitive-getdelim
    (ssize_t getdelim (void* void* int FILE*)))

  (define (getline S count-guess)
    (with-compensations
      (let ((pp	(compensate-malloc/small))
	    (pc	(compensate-malloc/small))
	    (p	(malloc count-guess)))
	(pointer-set-c-pointer!	pp 0 p)
	(pointer-set-c-int!	pc 0 count-guess)
	(receive (result errno)
	    (primitive-getline pp pc S)
	  (when (= -1 result)
	    (primitive-free p)
	    (raise-errno-error 'getline errno S count-guess))
	  (let ((q (pointer-ref-c-pointer pp 0))
;;;		(n (pointer-ref-c-signed-int pc 0))
		)
	    (begin0
		(cstring->string/len q result)
	      (primitive-free q)))))))

  (define (getdelim S delimiter count-guess)
    (with-compensations
      (let ((de (char->integer delimiter)))
	(when (< 255 de)
	  (assertion-violation 'getdelim
	    "expected delimiter with scalar value in range [0, 255]"
	    delimiter))
	(let ((pp	(compensate-malloc/small))
	      (pc	(compensate-malloc/small))
	      (p	(malloc count-guess)))
	  (pointer-set-c-pointer!	pp 0 p)
	  (pointer-set-c-int!	pc 0 count-guess)
	  (receive (result errno)
	      (primitive-getdelim pp pc de S)
	    (when (= -1 result)
	      (primitive-free p)
	      (raise-errno-error 'getline errno S count-guess))
	    (let ((q (pointer-ref-c-pointer pp 0))
		  (n (pointer-ref-c-signed-int pc 0)))
	      (begin0
		  (cstring->string/len q result)
		(primitive-free q))))))))

  (define-c-function fpurge
    (void __fpurge (FILE*))))

;;; end of file

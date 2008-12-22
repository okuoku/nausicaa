;;;
;;;Part of: Glibc libraries for R6RS Scheme
;;;Contents: extended stream functions
;;;Date: Thu Dec  4, 2008
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


;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (glibc streams-extended)
  (export
    freadable fwritable freading fwriting fwide fpurge

    getline primitive-getline primitive-getline-function platform-getline
    getdelim primitive-getdelim primitive-getdelim-function platform-getdelim)
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (glibc streams))

  (define libc
    (begin
      (shared-object self-shared-object)
      self-shared-object))


;;;; misc

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

(define-c-function fpurge
  (void __fpurge (FILE*)))



;;;; getting lines

(define-c-function/with-errno platform-getline
  (ssize_t getline (void* void* FILE*)))

(define-c-function/with-errno platform-getdelim
  (ssize_t getdelim (void* void* int FILE*)))

;;; --------------------------------------------------------------------

(define (primitive-getline stream)
  (with-compensations
    (let* ((*pointer	(malloc-small/c))
	   (*count	(malloc-small/c))
	   (getp	(lambda ()
			  (pointer-ref-c-pointer *pointer 0)))
	   (free	(lambda ()
			  (let ((p (getp)))
			    (unless (pointer-null? p)
			      (platform-free p))))))
      (receive (result errno)
	  (platform-getline *pointer *count stream)
	(cond ((ferror stream)
	       (free)
	       (raise-errno-error 'primitive-getline errno stream))
	      ((= -1 result)
	       (free)
	       "")
	      (else
	       (let ((p (getp)))
		 (begin0
		     (cstring->string/len p result)
		   (platform-free p)))))))))

(define (primitive-getdelim stream delimiter)
  (let ((delimiter (char->integer delimiter)))
    (when (< 255 delimiter)
      (assertion-violation 'primitive-getdelim
	"expected delimiter with scalar value in range [0, 255]"
	delimiter))
    (with-compensations
      (let* ((*pointer	(malloc-small/c))
	     (*count		(malloc-small/c))
	     (getp		(lambda ()
				  (pointer-ref-c-pointer *pointer 0)))
	     (free		(lambda ()
				  (let ((p (getp)))
				    (unless (pointer-null? p)
				      (platform-free p))))))
	(receive (result errno)
	    (platform-getdelim *pointer *count delimiter stream)
	  (cond ((ferror stream)
		 (free)
		 (raise-errno-error 'primitive-getline errno stream))
		((= -1 result)
		 (free)
		 "")
		(else
		 (let ((p (getp)))
		   (begin0
		       (cstring->string/len p result)
		     (platform-free p))))))))))

;;; --------------------------------------------------------------------

(define primitive-getline-function
  (make-parameter primitive-getline
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-getline-function
	  "expected procedure as value of the PRIMITIVE-GETLINE-FUNCTION parameter"
	  func))
      func)))

(define primitive-getdelim-function
  (make-parameter primitive-getdelim
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-getdelim-function
	  "expected procedure as value of the PRIMITIVE-GETDELIM-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (getline stream)
  ((primitive-getline-function) stream))

(define (getdelim stream delimiter)
  ((primitive-getdelim-function) stream delimiter))



;;;; done

)

;;; end of file

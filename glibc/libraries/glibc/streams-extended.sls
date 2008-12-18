;;;
;;;Part of: Glibc libraries for R6RS Scheme
;;;Contents: extended stream functions
;;;Date: Thu Dec  4, 2008
;;;Time-stamp: <2008-12-18 21:26:34 marco>
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
  (import (r6rs)
    (uriel lang)
    (uriel memory)
    (uriel ffi)
    (uriel cstring)
    (uriel errno)
    (srfi receive)
    (glibc streams))

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

  (define (getline stream)
    (with-compensations
      (let* ((*pointer	(malloc-small/c))
	     (*count	(malloc-small/c))
	     (getp	(lambda ()
			  (pointer-ref-c-pointer *pointer 0)))
	     (free	(lambda ()
			  (let ((p (getp)))
			    (unless (pointer-null? p)
			      (primitive-free p))))))
	(receive (result errno)
	    (primitive-getline *pointer *count stream)
	  (cond ((ferror stream)
		 (free)
		 (raise-errno-error 'getline errno stream))
		((= -1 result)
		 (free)
		 "")
		(else
		 (let ((p (getp)))
		   (begin0
		       (cstring->string/len p result)
		     (primitive-free p)))))))))

  (define (getdelim stream delimiter)
    (let ((delimiter (char->integer delimiter)))
      (when (< 255 delimiter)
	(assertion-violation 'getdelim
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
				      (primitive-free p))))))
	  (receive (result errno)
	      (primitive-getdelim *pointer *count delimiter stream)
	    (cond ((ferror stream)
		   (free)
		   (raise-errno-error 'getline errno stream))
		  ((= -1 result)
		   (free)
		   "")
		  (else
		   (let ((p (getp)))
		     (begin0
			 (cstring->string/len p result)
		       (primitive-free p))))))))))

  (define-c-function fpurge
    (void __fpurge (FILE*))))

;;; end of file

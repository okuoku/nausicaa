;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: interface to file functions
;;;Date: Sat Jan  3, 2009
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



;;;; setup

(library (name)
  (export a)
  (import (r6rs))


;;;; temporary files

(define (primitive-tempnam directory prefix)
  (with-compensations
    (let ((p	(platform-tmpnam (string->cstring/c directory)
				 (string->cstring/c prefix))))
      (begin0
	  (cstring->string p)
	(primitive-free p)))))

(define (primitive-mkdtemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform-mkdtemp p)
	(when (pointer-null? result)
	  (raise-errno-error 'primitive-mkdtemp errno template))
	(cstring->string p)))))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-tempnam-function primitive-tempnam)

(define-primitive-parameter
  primitive-mkdtemp-function primitive-mkdtemp)

;;; --------------------------------------------------------------------

(define (tempnam directory prefix)
  ((primitive-tempnam-function) directory prefix))

(define (mkdtemp template)
  ((primitive-mkdtemp-function) template))



;;;; file times

(define (real-utimes func funcname obj
		     access-time-sec access-time-usec
		     modification-time-sec modification-time-usec)
  (with-compensations
    (let* ((*arry	(malloc-block/c (* 2 strideof-struct-timeval)))
	   (*atime	*arry)
	   (*mtime	(pointer-add *arry strideof-struct-timeval)))
      (struct-timeval-tv_sec-set!  *atime access-time-sec)
      (struct-timeval-tv_usec-set! *atime access-time-usec)
      (struct-timeval-tv_sec-set!  *mtime modification-time-sec)
      (struct-timeval-tv_usec-set! *mtime modification-time-usec)
      (receive (result errno)
	  (func *arry)
	(when (= -1 result)
	  (raise-errno-error funcname errno
			     (list obj
				   access-time-sec access-time-usec
				   modification-time-sec modification-time-usec)))
	result))))

(define (primitive-utimes pathname
			  access-time-sec access-time-usec
			  modification-time-sec modification-time-usec)
  (real-utimes (lambda (*arry)
		 (platform-utimes (string->cstring/c pathname) *arry))
	       'primitive-utimes pathname
	       access-time-sec access-time-usec
	       modification-time-sec modification-time-usec))

(define (primitive-lutimes pathname
			   access-time-sec access-time-usec
			   modification-time-sec modification-time-usec)
  (real-utimes (lambda (*arry)
		 (platform-lutimes (string->cstring/c pathname) *arry))
	       'primitive-lutimes pathname
	       access-time-sec access-time-usec
	       modification-time-sec modification-time-usec))

(define (primitive-futimes fd
			   access-time-sec access-time-usec
			   modification-time-sec modification-time-usec)
  (real-utimes (lambda (*arry)
		 (platform-futimes fd *arry))
	       'primitive-futimes fd
	       access-time-sec access-time-usec
	       modification-time-sec modification-time-usec))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-utimes-function primitive-utimes)

(define-primitive-parameter
  primitive-lutimes-function primitive-lutimes)

(define-primitive-parameter
  primitive-futimes-function primitive-futimes)

;;; --------------------------------------------------------------------

(define (utimes pathname
		access-time-sec access-time-usec
		modification-time-sec modification-time-usec)
  ((primitive-utimes-function) pathname
   access-time-sec access-time-usec
   modification-time-sec modification-time-usec))

(define (lutimes pathname
		 access-time-sec access-time-usec
		 modification-time-sec modification-time-usec)
  ((primitive-lutimes-function) pathname
   access-time-sec access-time-usec
   modification-time-sec modification-time-usec))

(define (futimes fd
		 access-time-sec access-time-usec
		 modification-time-sec modification-time-usec)
  ((primitive-futimes-function) fd
   access-time-sec access-time-usec
   modification-time-sec modification-time-usec))


;;;; done

)

;;; end of file

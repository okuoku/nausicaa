;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface for file time functions
;;;Date: Fri Nov  6, 2009
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


(library (foreign glibc file primitives)
  (export
    tempnam		mkdtemp
    utimes		lutimes		futimes)
  (import (rnrs)
    (receive)
    (begin0)
    (compensations)
    (only (foreign pointers)
	  pointer-add
	  pointer-null?)
    (only (foreign cstrings)
	  string->cstring/c
	  cstring->string)
    (only (foreign errno)
	  raise-errno-error)
    (foreign glibc file platform))


(define (tempnam directory prefix)
  (with-compensations
    (let ((p (platform:tmpnam (string->cstring/c directory)
			      (string->cstring/c prefix))))
      (begin0
	  (cstring->string p)
	(free p)))))

(define (mkdtemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform:mkdtemp p)
	(when (pointer-null? result)
	  (raise-errno-error 'mkdtemp errno template))
	(cstring->string p)))))


(define (%real-utimes func funcname obj
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

(define (utimes pathname
		access-time-sec access-time-usec
		modification-time-sec modification-time-usec)
  (%real-utimes (lambda (*arry)
		  (platform:utimes (string->cstring/c pathname) *arry))
		'utimes pathname
		access-time-sec access-time-usec
		modification-time-sec modification-time-usec))

(define (lutimes pathname
		 access-time-sec access-time-usec
		 modification-time-sec modification-time-usec)
  (%real-utimes (lambda (*arry)
		  (platform:lutimes (string->cstring/c pathname) *arry))
		'lutimes pathname
		access-time-sec access-time-usec
		modification-time-sec modification-time-usec))

(define (futimes fd
		 access-time-sec access-time-usec
		 modification-time-sec modification-time-usec)
  (%real-utimes (lambda (*arry)
		  (platform:futimes fd *arry))
		'futimes fd
		access-time-sec access-time-usec
		modification-time-sec modification-time-usec))


;;;; done

)

;;; end of file

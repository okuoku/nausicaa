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

    ;; temporary files
    mktemp		mkstemp		mkdtemp
    tempnam		tmpnam		tmpfile

    utimes		lutimes		futimes)
  (import (except (rnrs)
		  remove truncate)
    (receive)
    (begin0)
    (compensations)
    (only (foreign ffi pointers)
	  pointer-add
	  pointer-null
	  pointer-null?)
    (only (foreign memory)
	  malloc-block/c
	  primitive-free)
    (only (foreign cstrings)
	  strlen
	  string->cstring/c
	  cstring->string)
    (only (foreign errno)
	  raise-errno-error)
    (foreign posix sizeof)
    (prefix (foreign glibc file platform) platform:))


(define (mktemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform:mktemp p)
	(if (or (pointer-null? result) (= 0 (strlen p)))
	    (raise-errno-error 'mktemp errno template)
	  (cstring->string p))))))

(define (mkstemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform:mkstemp p)
	(if (= -1 result)
	    (raise-errno-error 'mktemp errno template)
	  (values result (cstring->string p)))))))

(define (mkdtemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform:mkdtemp p)
	(if (pointer-null? result)
	    (raise-errno-error 'mkdtemp errno template)
	  (cstring->string p))))))

(define (tmpfile)
  (receive (result errno)
      (platform:tmpfile)
    (when (pointer-null? result)
      (raise-errno-error 'tmpfile errno))
    result))

(define (tempnam directory prefix)
  (with-compensations
    (receive (result errno)
	(platform:tempnam (if (not directory)
			      pointer-null
			    (string->cstring/c directory))
			  (if (not prefix)
			      pointer-null
			    (string->cstring/c prefix)))
      (if (pointer-null? result)
	  (raise-errno-error 'tempnam errno (list directory prefix))
	(begin0
	    (cstring->string result)
	  (primitive-free result))))))

(define (tmpnam)
  (with-compensations
    (receive (result errno)
	(platform:tmpnam (malloc-block/c (+ 1 L_tmpnam)))
      (if (pointer-null? result)
	  (raise-errno-error 'tmpnam errno)
	(cstring->string result)))))


(define %real-utimes
  (case-lambda
   ((func funcname obj
	  access-time-sec	    access-time-usec
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
   ((func funcname obj)
    (receive (result errno)
	(func pointer-null)
      (if (= -1 result)
	  (raise-errno-error funcname errno obj)
	result)))))

(define utimes
  (case-lambda
   ((pathname access-time-sec access-time-usec modification-time-sec modification-time-usec)
    (%real-utimes (lambda (*arry)
		    (platform:utimes (string->cstring/c pathname) *arry))
		  'utimes pathname
		  access-time-sec access-time-usec
		  modification-time-sec modification-time-usec))
   ((pathname)
    (%real-utimes (lambda (*arry)
		    (platform:utimes (string->cstring/c pathname) *arry))
		  'utimes pathname))))

(define lutimes
  (case-lambda
   ((pathname access-time-sec access-time-usec modification-time-sec modification-time-usec)
    (%real-utimes (lambda (*arry)
		    (platform:lutimes (string->cstring/c pathname) *arry))
		  'lutimes pathname
		  access-time-sec access-time-usec
		  modification-time-sec modification-time-usec))
   ((pathname)
    (%real-utimes (lambda (*arry)
		    (platform:lutimes (string->cstring/c pathname) *arry))
		  'lutimes pathname))))

(define futimes
  (case-lambda
   ((fd access-time-sec access-time-usec modification-time-sec modification-time-usec)
    (%real-utimes (lambda (*arry)
		    (platform:futimes fd *arry))
		  'futimes fd
		  access-time-sec access-time-usec
		  modification-time-sec modification-time-usec))
   ((fd)
    (%real-utimes (lambda (*arry)
		    (platform:futimes fd *arry))
		  'futimes fd))))


;;;; done

)

;;; end of file

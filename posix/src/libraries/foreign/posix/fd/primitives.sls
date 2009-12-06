;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling functions for fd functions
;;;Date: Wed Nov  4, 2009
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


(library (foreign posix fd primitives)
  (export
    integer->file-descriptor
    file-descriptor->integer
    file-descriptor?

    open	close
    read	write
    pread	pwrite
    lseek
    sync	fsync
    fdatasync
    fcntl	ioctl
    dup	dup2
    pipe	mkfifo)
  (import (except (rnrs) read write)
    (receive)
    (compensations)
    (only (foreign memory)
	  malloc-block/c)
    (only (foreign cstrings)
	  string->cstring/c)
    (only (foreign ffi peekers-and-pokers)
	  array-ref-c-signed-int)
    (only (foreign errno)
	  EINTR raise-errno-error)
    (only (foreign ffi sizeof)
	  sizeof-int-array)
    (prefix (foreign posix fd platform) platform:)
    (foreign posix wrappers))


;;;; helpers

(define-syntax %temp-failure-retry-minus-one
  ;;Invoke a "/with-errno" function again  and again until its result is
  ;;different from -1, or the error is not EINTR.
  ;;
  ;;It is  a syntax in an  attempt to speed  up things, but it  could be
  ;;made into a function.
  ;;
  (syntax-rules ()
    ((_ ?funcname (?primitive ?arg ...) ?irritants)
     (let loop ()
       (receive (result errno)
	   (?primitive ?arg ...)
	 (when (= -1 result)
	   (when (= EINTR errno)
	     (loop))
	   (raise-errno-error (quote ?funcname) errno ?irritants))
	 result)))))

(define-syntax %call-for-minus-one
  ;;Invoke a "/with-errno" function; if  the return value is "-1", it is
  ;;interpreted as an error, so an "&errno" exception is raised.
  ;;
  ;;This macro is  for platform functions that are  NOT interrupted by a
  ;;EINTR.
  ;;
  (syntax-rules ()
    ((_ ?funcname ?primitive ?arg ...)
     (receive (result errno)
	 (?primitive ?arg ...)
       (when (= -1 result)
	 (raise-errno-error (quote ?funcname) errno (list ?arg ...)))
       result))))


;;;; opening and closing

(define (open pathname open-mode permissions)
  (integer->file-descriptor (with-compensations
			      (%temp-failure-retry-minus-one
			       open
			       (platform:open (string->cstring/c pathname) open-mode permissions)
			       (list pathname open-mode permissions)))))

(define (close fd)
  (%temp-failure-retry-minus-one close (platform:close (file-descriptor->integer fd)) fd))


;;;; reading and writing

(define-syntax %do-read-or-write
  (syntax-rules ()
    ((_ ?funcname ?primitive ?fd ?pointer ?number-of-bytes)
     (%temp-failure-retry-minus-one
      ?funcname
      (?primitive (file-descriptor->integer ?fd) ?pointer ?number-of-bytes)
      ?fd))))

(define-syntax %do-pread-or-pwrite
  (syntax-rules ()
    ((_ ?funcname ?primitive ?fd ?pointer ?number-of-bytes ?offset)
     (%temp-failure-retry-minus-one
      ?funcname
      (?primitive (file-descriptor->integer ?fd) ?pointer ?number-of-bytes ?offset)
      ?fd))))

(define (read fd pointer number-of-bytes)
  (%do-read-or-write read platform:read fd pointer number-of-bytes))

(define (write fd pointer number-of-bytes)
  (%do-read-or-write write platform:write fd pointer number-of-bytes))

(define (pread fd pointer number-of-bytes offset)
  (%do-pread-or-pwrite pread platform:pread fd pointer number-of-bytes offset))

(define (pwrite fd pointer number-of-bytes offset)
  (%do-pread-or-pwrite pwrite platform:pwrite fd pointer number-of-bytes offset))


;;;; seeking

(define (lseek fd offset whence)
  ;;It seems  that EINTR  cannot happen with  "lseek()", but it  does no
  ;;harm to use the macro.
  (%temp-failure-retry-minus-one lseek (platform:lseek (file-descriptor->integer fd) offset whence) fd))


;;;; synchronisation

(define (sync)
  (receive (result errno)
      (platform:sync)
    (unless (= 0 result)
      (raise-errno-error 'sync errno #f))
    result))

(define (fsync fd)
  (%call-for-minus-one fsync platform:fsync (file-descriptor->integer fd)))

(define (fdatasync fd)
  (%call-for-minus-one fdatasync platform:fdatasync (file-descriptor->integer fd)))


;;;; control operations

(define (fcntl fd operation arg)
  (%call-for-minus-one fcntl platform:fcntl (file-descriptor->integer fd) operation arg))

(define (ioctl fd operation arg)
  (%call-for-minus-one ioctl platform:ioctl (file-descriptor->integer fd) operation arg))


;;;; duplicating

(define (dup fd)
  (integer->file-descriptor (%call-for-minus-one dup platform:dup (file-descriptor->integer fd))))

(define (dup2 old new)
  (integer->file-descriptor (%call-for-minus-one dup2 platform:dup2
						 (file-descriptor->integer old)
						 new)))


;;;; making pipes

(define (pipe)
  (with-compensations
    (let ((p (malloc-block/c (sizeof-int-array 2))))
      (receive (result errno)
	  (platform:pipe p)
	(if (= -1 result)
	    (raise-errno-error 'pipe errno)
	  (values (integer->file-descriptor (array-ref-c-signed-int p 0))
		  (integer->file-descriptor (array-ref-c-signed-int p 1))))))))

(define (mkfifo pathname mode)
  (with-compensations
    ;;Here we do not use  %CALL-FOR-MINUS-ONE because we have to marshal
    ;;the first argument.
    (receive (result errno)
	(platform:mkfifo (string->cstring/c pathname) mode)
      (if (= -1 result)
	  (raise-errno-error 'mkfifo errno (list pathname mode))
	result))))


;;;; done

)

;;; end of file

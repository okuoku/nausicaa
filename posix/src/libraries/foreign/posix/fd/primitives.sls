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
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    pipe	mkfifo
    readv	writev
    mmap	munmap		msync	mremap

    select FD_ISSET FD_SET FD_CLR FD_ZERO
;;;    aio-read aio-write aio-error aio-return aio-fsync aio-suspend aio-cancel lio-listio
    )
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
    (only (foreign ffi pointers)
	  pointer?
	  pointer=?
	  integer->pointer
	  pointer->integer)
    (only (foreign ffi sizeof)
	  sizeof-int-array)
    (only (foreign posix sizeof)
	  sizeof-fdset)
    (foreign posix typedefs)
    (prefix (foreign posix fd platform) platform:))


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
  (%call-for-minus-one fcntl
		       (if (or (pointer? arg)
			       (struct-flock? arg))
			   platform:fcntl/ptr
			 platform:fcntl)
		       (file-descriptor->integer fd)
		       operation
		       (cond ((pointer? arg)
			      arg)
			     ((struct-flock? arg)
			      (struct-flock->pointer arg))
			     (else
			      arg))))

(define (ioctl fd operation arg)
  (%call-for-minus-one ioctl platform:ioctl (file-descriptor->integer fd) operation arg))


;;;; duplicating

(define (dup fd)
  (integer->file-descriptor (%call-for-minus-one dup platform:dup (file-descriptor->integer fd))))

(define (dup2 old new)
  (integer->file-descriptor (%call-for-minus-one dup2 platform:dup2
						 (file-descriptor->integer old)
						 (if (file-descriptor? new)
						     (file-descriptor->integer new)
						   new))))


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


;;;; scatter/gather reading and writing

(define (readv fd buffers buffer-count)
  (receive (bytes-read errno)
      (platform:readv (file-descriptor->integer fd) buffers buffer-count)
    (if (= -1 bytes-read)
	(raise-errno-error 'readv errno (list fd buffers buffer-count))
      bytes-read)))

(define (writev fd buffers buffer-count)
  (receive (bytes-written errno)
      (platform:writev (file-descriptor->integer fd) buffers buffer-count)
    (if (= -1 bytes-written)
	(raise-errno-error 'writev errno (list fd buffers buffer-count))
      bytes-written)))


;;;; mmap

(define (mmap address length protect flags fd offset)
  (receive (effective-address errno)
      (platform:mmap address length protect flags (file-descriptor->integer fd) offset)
    ;;;(pointer=? effective-address (integer->pointer -1)) ;ugly, but what can I do?
    (if (let ((i (pointer->integer effective-address)))
	  (or (= -1 i) ;for ikarus and ypsilon
	      (= #xffffffff i)
	      (= #xffffffffffffffff i)))
	(raise-errno-error 'mmap errno (list address length protect flags fd offset))
      effective-address)))

(define (munmap address length)
  (receive (result errno)
      (platform:munmap address length)
    (if (= -1 result)
	(raise-errno-error 'munmap errno (list address length))
      result)))

(define (msync address length flags)
  (receive (result errno)
      (platform:msync address length flags)
    (if (= -1 result)
	(raise-errno-error 'msync errno (list address length flags))
      result)))

(define (mremap address length new-length flags)
  (receive (result errno)
      (platform:mremap address length new-length flags)
    (if (= -1 result)
	(raise-errno-error 'mremap errno (list address length new-length flags))
      result)))


;;; select

(define (FD_ZERO set)
  (platform:FD_ZERO (fdset->pointer set)))

(define (FD_ISSET fd set)
  (not (= 0 (platform:FD_ISSET (file-descriptor->integer fd) (fdset->pointer set)))))

(define (FD_SET fd set)
  (platform:FD_SET (file-descriptor->integer fd) (fdset->pointer set)))

(define (FD_CLR fd set)
  (platform:FD_CLR (file-descriptor->integer fd) (fdset->pointer set)))

(define (select max-fd read-fdset write-fdset except-fdset timeval)
  (receive (total-number-of-ready-fds errno)
      (platform:select (if (file-descriptor? max-fd)
			   (file-descriptor->integer max-fd)
			 max-fd)
		       (fdset->pointer read-fdset)
		       (fdset->pointer write-fdset)
		       (fdset->pointer except-fdset)
		       (struct-timeval->pointer timeval))
    (if (= -1 total-number-of-ready-fds)
	(raise-errno-error 'select errno (list max-fd read-fdset write-fdset except-fdset timeval))
      total-number-of-ready-fds)))


;;; asynchronous input/output

;; (define (aio-read aiocb)
;;   (receive (result errno)
;;       (platform:aio_read (struct-aciocb->pointer aciocb))
;;     (if (= -1 result)
;; 	(raise-errno-error 'aio-read errno aiocb)
;;       result)))

;; (define (aio_write)
;;   )

;; (define (lio_listio)
;;   )


;; (define (aio_error)
;;   )

;; (define (aio_return)
;;   )

;; (define (aio_fsync)
;;   )

;; (define (aio_suspend)
;;   )

;; (define (aio_cancel)
;;   )


;;;; done

)

;;; end of file

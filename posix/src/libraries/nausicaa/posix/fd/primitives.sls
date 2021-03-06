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
;;;Copyright (c) 2009-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix fd primitives)
  (export
    integer->fd	fd->integer	fd?

    open		close
    read		write
    pread		pwrite
    lseek
    sync		fsync
    fdatasync
    fcntl		ioctl
    dup	dup2
    pipe		mkfifo
    readv		writev

    mmap		munmap
    msync		mremap

    select		select/interruptible
    select*		select*/interruptible
    select/fd		select/fd/interruptible
    FD_ISSET		FD_SET
    FD_CLR		FD_ZERO

;;;aio-read aio-write aio-error aio-return aio-fsync aio-suspend aio-cancel lio-listio
    )
  (import (except (rnrs) read write)
    (nausicaa language extensions)
    (nausicaa language compensations)
    (prefix (only (nausicaa ffi memory)
		  malloc-block/c
		  malloc-small/c)
	    mem.)
    (prefix (only (nausicaa ffi cstrings)
		  string->cstring/c)
	    cstr.)
    (prefix (only (nausicaa ffi peekers-and-pokers)
		  array-c-ref)
	    ffi.)
    (prefix (only (nausicaa ffi errno)
		  EINTR
		  raise-errno-error)
	    errno.)
    (nausicaa ffi pointers)
    (prefix (nausicaa ffi sizeof) ffi.)
    (prefix (nausicaa posix sizeof) so.)
    (nausicaa posix typedefs)
    (only (nausicaa posix time primitives) <timeval>->pointer)
    (prefix (nausicaa posix fd platform) platform.))


;;;; helpers

(define-syntax %temp-failure-retry-minus-one
  ;;Invoke a "/with-errno" function again  and again until its result is
  ;;different from -1, or the error is not errno.EINTR.
  ;;
  ;;It is  a syntax in an  attempt to speed  up things, but it  could be
  ;;made into a function.
  ;;
  (syntax-rules ()
    ((_ ?who (?primitive ?arg ...) ?irritants)
     (let loop ()
       (receive (result errno)
	   (?primitive ?arg ...)
	 (when (= -1 result)
	   (when (= errno.EINTR errno)
	     (loop))
	   (errno.raise-errno-error (quote ?who) errno ?irritants))
	 result)))))

(define-syntax %call-for-minus-one
  ;;Invoke a "/with-errno" function; if  the return value is "-1", it is
  ;;interpreted as an error, so an "&errno" exception is raised.
  ;;
  ;;This macro is  for platform functions that are  NOT interrupted by a
  ;;errno.EINTR.
  ;;
  (syntax-rules ()
    ((_ ?who ?primitive ?arg ...)
     (receive (result errno)
	 (?primitive ?arg ...)
       (when (= -1 result)
	 (errno.raise-errno-error (quote ?who) errno (list ?arg ...)))
       result))))

(define-syntax %call-for-minus-one/irritants
  ;;Invoke a "/with-errno" function; if  the return value is "-1", it is
  ;;interpreted as an error, so an "&errno" exception is raised.
  ;;
  ;;This macro is  for platform functions that are  NOT interrupted by a
  ;;errno.EINTR.
  ;;
  (syntax-rules ()
    ((_ ?who (?primitive ?arg ...) ?irritants ...)
     (receive (result errno)
	 (?primitive ?arg ...)
       (when (= -1 result)
	 (errno.raise-errno-error (quote ?who) errno (list ?irritants ...)))
       result))))

(define-syntax %fd->integer
  (syntax-rules ()
    ((_ ?obj)
     (let ((obj ?obj))
       (if (fd? obj)
	   (fd->integer obj)
	 obj)))))

(define (%timeval->pointer/c obj who)
  (cond ((<timeval>? obj)
	 (<timeval>->pointer obj mem.malloc-block/c))
	((struct-timeval? obj)
	 (struct-timeval->pointer obj))
	((pointer? obj)
	 obj)
	(else
	 (assertion-violation who "expected struct timeval specification" obj))))


;;;; opening and closing

(define (%permissions->value permissions)
  (if (integer? permissions)
      permissions
    (access-permissions->value permissions)))

(define (%open-mode->value open-mode)
  (if (integer? open-mode)
      open-mode
    (open-mode->value open-mode)))

(define (open pathname open-mode permissions)
  (integer->fd (with-compensations
			      (%temp-failure-retry-minus-one
			       open
			       (platform.open (cstr.string->cstring/c pathname)
					      (%open-mode->value open-mode)
					      (%permissions->value permissions))
			       (list pathname open-mode permissions)))))

(define (close fd)
  (%temp-failure-retry-minus-one close (platform.close (fd->integer fd)) fd))


;;;; reading and writing

(define-syntax %do-read-or-write
  (syntax-rules ()
    ((_ ?funcname ?primitive ?fd ?pointer ?number-of-bytes)
     (%temp-failure-retry-minus-one
      ?funcname
      (?primitive (fd->integer ?fd) ?pointer ?number-of-bytes)
      ?fd))))

(define-syntax %do-pread-or-pwrite
  (syntax-rules ()
    ((_ ?funcname ?primitive ?fd ?pointer ?number-of-bytes ?offset)
     (%temp-failure-retry-minus-one
      ?funcname
      (?primitive (fd->integer ?fd) ?pointer ?number-of-bytes ?offset)
      ?fd))))

(define (read fd pointer number-of-bytes)
  (%do-read-or-write read platform.read fd pointer number-of-bytes))

(define (write fd pointer number-of-bytes)
  (%do-read-or-write write platform.write fd pointer number-of-bytes))

(define (pread fd pointer number-of-bytes offset)
  (%do-pread-or-pwrite pread platform.pread fd pointer number-of-bytes offset))

(define (pwrite fd pointer number-of-bytes offset)
  (%do-pread-or-pwrite pwrite platform.pwrite fd pointer number-of-bytes offset))


;;;; seeking

(define (lseek fd offset whence)
  ;;It seems that errno.EINTR cannot  happen with "lseek()", but it does
  ;;no harm to use the macro.
  (%temp-failure-retry-minus-one lseek
				 (platform.lseek (fd->integer fd) offset whence)
				 (list fd offset whence)))


;;;; synchronisation

(define (sync)
  (receive (result errno)
      (platform.sync)
    (unless (zero? result)
      (errno.raise-errno-error 'sync errno #f))
    result))

(define (fsync fd)
  (%call-for-minus-one/irritants fsync
				 (platform.fsync (fd->integer fd))
				 fd))

(define (fdatasync fd)
  (%call-for-minus-one/irritants fdatasync
				 (platform.fdatasync (fd->integer fd))
				 fd))


;;;; control operations

(define (fcntl fd operation arg)
  (%call-for-minus-one/irritants fcntl
				 ((if (or (pointer? arg)
					  (struct-flock? arg))
				      platform.fcntl/ptr
				    platform.fcntl)
				  (fd->integer fd)
				  operation
				  (cond ((pointer? arg)
					 arg)
					((struct-flock? arg)
					 (struct-flock->pointer arg))
					(else
					 arg)))
				 fd operation arg))

(define (ioctl fd operation arg)
  (%call-for-minus-one/irritants ioctl
				 (platform.ioctl (fd->integer fd) operation arg)
				 fd operation arg))


;;;; duplicating

(define (dup fd)
  (integer->fd (%call-for-minus-one/irritants dup
						(platform.dup (fd->integer fd))
						fd)))

(define (dup2 old new)
  (integer->fd (%call-for-minus-one/irritants dup2
						(platform.dup2 (fd->integer old)
							       (%fd->integer new))
						old new)))


;;;; making pipes

(define (pipe)
  (with-compensations
    (let ((p (mem.malloc-small/c)))
      (receive (result errno)
	  (platform.pipe p)
	(if (= -1 result)
	    (errno.raise-errno-error 'pipe errno)
	  (values (integer->fd (ffi.array-c-ref signed-int p 0))
		  (integer->fd (ffi.array-c-ref signed-int p 1))))))))

(define (mkfifo pathname mode)
  (with-compensations
    (%call-for-minus-one/irritants mkfifo
				   (platform.mkfifo (cstr.string->cstring/c pathname) mode)
				   pathname mode)))


;;;; scatter/gather reading and writing

(define (readv fd buffers buffer-count)
  (%call-for-minus-one/irritants readv
				 (platform.readv (fd->integer fd) buffers buffer-count)
				 fd buffers buffer-count))

(define (writev fd buffers buffer-count)
  (%call-for-minus-one/irritants writev
				 (platform.writev (fd->integer fd) buffers buffer-count)
				 fd buffers buffer-count))


;;;; mmap

(define (mmap address length protect flags fd offset)
  (receive (effective-address errno)
      (platform.mmap address length protect flags (fd->integer fd) offset)
    ;;;(pointer=? effective-address (integer->pointer -1)) ;ugly, but what can I do?
    (if (let ((i (pointer->integer effective-address)))
	  (or (= -1 i) ;for ikarus and ypsilon
	      (= #xffffffff i)
	      (= #xffffffffffffffff i)))
	(errno.raise-errno-error 'mmap errno (list address length protect flags fd offset))
      effective-address)))

(define (munmap address length)
  (%call-for-minus-one munmap platform.munmap address length))

(define (msync address length flags)
  (%call-for-minus-one msync platform.msync address length flags))

(define (mremap address length new-length flags)
  (%call-for-minus-one mremap platform.mremap address length new-length flags))


;;; select

(define (FD_ZERO set)
  (platform.FD_ZERO (fdset->pointer set)))

(define (FD_ISSET fd set)
  (not (zero? (platform.FD_ISSET (fd->integer fd) (fdset->pointer set)))))

(define (FD_SET fd set)
  (platform.FD_SET (fd->integer fd) (fdset->pointer set)))

(define (FD_CLR fd set)
  (platform.FD_CLR (fd->integer fd) (fdset->pointer set)))

(define-syntax %fdset-true-or-null
  (syntax-rules ()
    ((_ ?obj)
     (let ((obj ?obj))
       (if obj
	   (fdset->pointer obj)
	 pointer-null)))))

(define (select/interruptible max-fd read-fdset write-fdset except-fdset timeval)
  (with-compensations
    (%call-for-minus-one select platform.select
			 (if max-fd (%fd->integer max-fd) so.FD_SETSIZE)
			 (%fdset-true-or-null read-fdset)
			 (%fdset-true-or-null write-fdset)
			 (%fdset-true-or-null except-fdset)
			 (%timeval->pointer/c timeval 'select/interruptible))))

(define (select max-fd read-fdset write-fdset except-fdset timeval)
  (with-compensations
    (%temp-failure-retry-minus-one
     select
     (platform.select (if max-fd (%fd->integer max-fd) so.FD_SETSIZE)
		      (%fdset-true-or-null read-fdset)
		      (%fdset-true-or-null write-fdset)
		      (%fdset-true-or-null except-fdset)
		      (%timeval->pointer/c timeval 'select))
     (list max-fd read-fdset write-fdset except-fdset timeval))))

;;; --------------------------------------------------------------------

(define (select* rd-ell wr-ell ex-ell timeval)
  (assert (list? rd-ell))
  (assert (list? wr-ell))
  (assert (list? ex-ell))
  (with-compensations
    (let* ((pool*	(mem.malloc-block/c (so.c-sizeof fdset 3)))
	   (rd-set*	(so.array-c-ref fdset pool* 0))
	   (wr-set*	(so.array-c-ref fdset pool* 1))
	   (ex-set*	(so.array-c-ref fdset pool* 2)))
      (platform.FD_ZERO rd-set*)
      (platform.FD_ZERO wr-set*)
      (platform.FD_ZERO ex-set*)
      (for-each (lambda (fdset* fd-ell)
		  (for-each (lambda (fd)
			      (platform.FD_SET (fd->integer fd) fdset*))
		    fd-ell))
	(list rd-set* wr-set* ex-set*)
	(list rd-ell  wr-ell  ex-ell))
      (let ((total-number-of-fds
	     (%temp-failure-retry-minus-one
	      select
	      (platform.select so.FD_SETSIZE
			       rd-set* wr-set* ex-set*
			       (%timeval->pointer/c timeval 'select*))
	      (list rd-ell wr-ell ex-ell timeval))))
	(if (zero? total-number-of-fds)
	    (values '() '() '())
	  (let ((%fold-fds (lambda (fd-set* fd-ell)
			     (fold-left (lambda (knil fd)
					  (if (zero? (platform.FD_ISSET (fd->integer fd) fd-set*))
					      knil
					    (cons fd knil)))
					'()
					fd-ell))))
	    (values (%fold-fds rd-set* rd-ell)
		    (%fold-fds wr-set* wr-ell)
		    (%fold-fds ex-set* ex-ell))))))))

(define (select*/interruptible rd-ell wr-ell ex-ell timeval)
  (assert (list? rd-ell))
  (assert (list? wr-ell))
  (assert (list? ex-ell))
  (with-compensations
    (let* ((pool*	(mem.malloc-block/c (so.c-sizeof fdset 3)))
	   (rd-set*	(so.array-c-ref fdset pool* 0))
	   (wr-set*	(so.array-c-ref fdset pool* 1))
	   (ex-set*	(so.array-c-ref fdset pool* 2)))
      (platform.FD_ZERO rd-set*)
      (platform.FD_ZERO wr-set*)
      (platform.FD_ZERO ex-set*)
      (for-each (lambda (fdset* fd-ell)
		  (for-each (lambda (fd)
			      (platform.FD_SET (fd->integer fd) fdset*))
		    fd-ell))
	(list rd-set* wr-set* ex-set*)
	(list rd-ell  wr-ell  ex-ell))
      (let ((total-number-of-fds
	     (%call-for-minus-one select platform.select
				  so.FD_SETSIZE
				  rd-set* wr-set* ex-set*
				  (%timeval->pointer/c timeval 'select*/interruptible))))
	(if (zero? total-number-of-fds)
	    (values '() '() '())
	  (let ((%fold-fds (lambda (fd-set* fd-ell)
			     (fold-left (lambda (knil fd)
					  (if (zero? (platform.FD_ISSET (fd->integer fd) fd-set*))
					      knil
					    (cons fd knil)))
					'()
					fd-ell))))
	    (values (%fold-fds rd-set* rd-ell)
		    (%fold-fds wr-set* wr-ell)
		    (%fold-fds ex-set* ex-ell))))))))

;;; --------------------------------------------------------------------

(define (select/fd fd timeval)
  (assert (fd? fd))
  (with-compensations
    (let* ((pool*	(mem.malloc-block/c (so.c-sizeof fdset 3)))
	   (rd-set*	(so.array-c-ref fdset pool* 0))
	   (wr-set*	(so.array-c-ref fdset pool* 1))
	   (ex-set*	(so.array-c-ref fdset pool* 2))
	   (fd-int	(fd->integer fd)))
      (platform.FD_ZERO rd-set*)
      (platform.FD_ZERO wr-set*)
      (platform.FD_ZERO ex-set*)
      (platform.FD_SET fd-int rd-set*)
      (platform.FD_SET fd-int wr-set*)
      (platform.FD_SET fd-int ex-set*)
      (let ((total-number-of-fds
	     (%temp-failure-retry-minus-one
	      select
	      (platform.select so.FD_SETSIZE
			       rd-set* wr-set* ex-set*
			       (%timeval->pointer/c timeval 'select/fd))
	      (list fd timeval))))
	(if (zero? total-number-of-fds)
	    (values #f #f #f)
	  (values (not (zero? (platform.FD_ISSET fd-int rd-set*)))
		  (not (zero? (platform.FD_ISSET fd-int wr-set*)))
		  (not (zero? (platform.FD_ISSET fd-int ex-set*)))))))))

(define (select/fd/interruptible fd timeval)
  (assert (fd? fd))
  (with-compensations
    (let* ((pool*	(mem.malloc-block/c (so.c-sizeof fdset 3)))
	   (rd-set*	(so.array-c-ref fdset pool* 0))
	   (wr-set*	(so.array-c-ref fdset pool* 1))
	   (ex-set*	(so.array-c-ref fdset pool* 2))
	   (fd-int	(fd->integer fd)))
      (platform.FD_ZERO rd-set*)
      (platform.FD_ZERO wr-set*)
      (platform.FD_ZERO ex-set*)
      (platform.FD_SET fd-int rd-set*)
      (platform.FD_SET fd-int wr-set*)
      (platform.FD_SET fd-int ex-set*)
      (let ((total-number-of-fds
	     (%call-for-minus-one select platform.select
				  so.FD_SETSIZE
				  rd-set* wr-set* ex-set*
				  (%timeval->pointer/c timeval 'select/fd))))
	(if (zero? total-number-of-fds)
	    (values #f #f #f)
	  (values (not (zero? (platform.FD_ISSET fd-int rd-set*)))
		  (not (zero? (platform.FD_ISSET fd-int wr-set*)))
		  (not (zero? (platform.FD_ISSET fd-int ex-set*)))))))))


;;; asynchronous input/output

;; (define (aio-read aiocb)
;;   (receive (result errno)
;;       (platform.aio_read (struct-aciocb->pointer aciocb))
;;     (if (= -1 result)
;; 	(errno.raise-errno-error 'aio-read errno aiocb)
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

;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to the file descriptor libraries
;;;Date: Fri Dec  5, 2008
;;;Time-stamp: <2008-12-07 20:22:23 marco>
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



;;;; setup

(library (posix fd)
  (export
    open		primitive-open
    close		primitive-close

    read		primitive-read
    write		primitive-write
    pread		primitive-pread
    pwrite		primitive-pwrite

    lseek		primitive-lseek

    sync		primitive-sync
    fsync		primitive-fsync
    fdatasync		primitive-fdatasync

    fcntl		primitive-fcntl
    ioctl		primitive-ioctl

    dup			primitive-dup
    dup2		primitive-dup2
    )
  (import (except (rnrs) read write)
    (srfi receive)
    (uriel lang)
    (rename (uriel ffi)
	    (string-or-symbol->cstring/compensated s->c))
    (uriel ffi sizeof)
    (uriel ffi errno)
    (posix sizeof))



;;;; helpers

;;Invoke  a "/with-errno"  thunk again  and  again until  its result  is
;;different from -1, or the error is not EINTR.
;;
;;It is a syntax in an attempt  to speed up things, but it could be made
;;into a function.
(define-syntax temp-failure-retry-minus-one
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

(define-syntax call-for-minus-one
  (syntax-rules ()
    ((_ ?funcname ?primitive ?arg ...)
     (receive (result errno)
	 (?primitive ?arg ...)
       (when (= -1 result)
	 (raise-errno-error (quote ?funcname) errno (list ?arg ...)))
       result))))



;;;; opening and closing

(define-c-function/with-errno primitive-open
  (int open (char* int mode_t)))

(define-c-function/with-errno primitive-close
  (int close (int)))


(define (open pathname open-mode permissions)
  (with-compensations
    (let ((pathname (s->c pathname)))
      (temp-failure-retry-minus-one
       'open
       (primitive-open pathname open-mode permissions)
       (list pathname open-mode permissions)))))

(define (close fd)
  (temp-failure-retry-minus-one
   'close
   (primitive-close fd)
   fd))



;;;; reading and writing

(define-c-function/with-errno primitive-read
  (ssize_t read (int void* size_t)))

(define-c-function/with-errno primitive-pread
  (ssize_t pread (int void* size_t off_t)))

(define-c-function/with-errno primitive-write
  (ssize_t write (int void* size_t)))

(define-c-function/with-errno primitive-pwrite
  (ssize_t pwrite (int void* size_t off_t)))

(define-syntax do-read-or-write
  (syntax-rules ()
    ((_ ?funcname ?primitive ?fd ?pointer ?number-of-bytes)
     (temp-failure-retry-minus-one
      ?funcname
      (?primitive ?fd ?pointer ?number-of-bytes)
      ?fd))))

(define (do-pread-or-pwrite ?funcname ?primitive ?fd ?pointer ?number-of-bytes ?offset)
  (temp-failure-retry-minus-one
   ?funcname
   (?primitive ?fd ?pointer ?number-of-bytes ?offset)
   ?fd))

(define (read fd pointer number-of-bytes)
  (do-read-or-write 'read primitive-read fd pointer number-of-bytes))

(define (write fd pointer number-of-bytes)
  (do-read-or-write 'write primitive-write fd pointer number-of-bytes))

(define (pread fd pointer number-of-bytes offset)
  (do-pread-or-pwrite 'pread primitive-pread fd pointer number-of-bytes offset))

(define (pwrite fd pointer number-of-bytes offset)
  (do-pread-or-pwrite 'pwrite primitive-pwrite fd pointer number-of-bytes offset))



;;;; seeking

(define-c-function/with-errno primitive-lseek
  (off_t lseek (int off_t int)))


(define (lseek fd offset whence)
  ;;It seems  that EINTR  cannot happen with  "lseek()", but it  does no
  ;;harm to use the macro.
  (temp-failure-retry-minus-one
   'lseek
   (primitive-lseek fd offset whence)
   fd))



;;;; synchronisation

(define-c-function/with-errno primitive-sync
  (int sync (void)))

(define-c-function/with-errno primitive-fsync
  (int fsync (int)))

(define-c-function/with-errno primitive-fdatasync
  (int fdatasync (int)))

(define (sync)
  (receive (result errno)
      (primitive-sync)
    (unless (= 0 result)
      (raise-errno-error 'sync errno #f))
    result))

(define (fsync fd)
  (call-for-minus-one fsync primitive-fsync fd))

(define (fdatasync fd)
  (call-for-minus-one fdatasync primitive-fdatasync fd))



;;;; control operations

(define-c-function/with-errno primitive-fcntl
  (int fcntl (int int int)))

(define-c-function/with-errno primitive-ioctl
  (int ioctl (int int int)))

(define (fcntl fd operation arg)
  (call-for-minus-one fcntl primitive-fcntl fd operation arg))

(define (ioctl fd operation arg)
  (call-for-minus-one ioctl primitive-ioctl fd operation arg))



;;;; duplicating

(define-c-function/with-errno primitive-dup
  (int dup (int)))

(define-c-function/with-errno primitive-dup2
  (int dup2 (int int)))

(define (dup fd)
  (call-for-minus-one dup primitive-dup fd))

(define (dup2 old new)
  (call-for-minus-one dup2 primitive-dup2 old new))



;;;; done

)

;;; end of file

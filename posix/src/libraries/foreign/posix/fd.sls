;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to the file descriptor libraries
;;;Date: Fri Dec  5, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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


#!r6rs
(library (foreign posix fd)
  (export
    open	primitive-open		primitive-open-function
    close	primitive-close		primitive-close-function

    read	primitive-read		primitive-read-function
    write	primitive-write		primitive-write-function
    pread	primitive-pread		primitive-pread-function
    pwrite	primitive-pwrite	primitive-pwrite-function

    lseek	primitive-lseek		primitive-lseek-function

    sync	primitive-sync		primitive-sync-function
    fsync	primitive-fsync		primitive-fsync-function
    fdatasync	primitive-fdatasync	primitive-fdatasync-function

    fcntl	primitive-fcntl		primitive-fcntl-function
    ioctl	primitive-ioctl		primitive-ioctl-function

    dup		primitive-dup		primitive-dup-function
    dup2	primitive-dup2		primitive-dup2-function

    pipe	primitive-pipe		primitive-pipe-function
    mkfifo	primitive-mkfifo	primitive-mkfifo-function

    fd->binary-input-port		fd->textual-input-port
    fd->binary-output-port		fd->textual-output-port
    fd->binary-input/ouput-port		fd->textual-input/ouput-port

    pipe-binary-ports			pipe-textual-ports)
  (import (except (nausicaa)
		  read write)
    (rnrs mutable-strings)
    (foreign ffi)
    (foreign ffi peekers-and-pokers)	;to be removed
    (foreign ffi sizeof)
    (foreign memory)
    (foreign errno)
    (foreign cstrings)
    (foreign posix sizeof)
    (foreign posix fd platform)
    (compensations))

  (define dummy
    (shared-object self-shared-object))


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

(define (primitive-open pathname open-mode permissions)
  (with-compensations
    (temp-failure-retry-minus-one
     primitive-open
     (platform-open (string->cstring/c pathname) open-mode permissions)
     (list pathname open-mode permissions))))

(define-primitive-parameter
  primitive-open-function primitive-open)

(define (open pathname open-mode permissions)
  ((primitive-open-function) pathname open-mode permissions))

;;; --------------------------------------------------------------------

(define (primitive-close fd)
  (temp-failure-retry-minus-one
   primitive-close
   (platform-close fd)
   fd))

(define-primitive-parameter
  primitive-close-function primitive-close)

(define (close fd)
  ((primitive-close-function) fd))



;;;; reading and writing

(define-syntax do-read-or-write
  (syntax-rules ()
    ((_ ?funcname ?primitive ?fd ?pointer ?number-of-bytes)
     (temp-failure-retry-minus-one
      ?funcname
      (?primitive ?fd ?pointer ?number-of-bytes)
      ?fd))))

(define-syntax do-pread-or-pwrite
  (syntax-rules ()
    ((_ ?funcname ?primitive ?fd ?pointer ?number-of-bytes ?offset)
     (temp-failure-retry-minus-one
      ?funcname
      (?primitive ?fd ?pointer ?number-of-bytes ?offset)
      ?fd))))

(define (primitive-read fd pointer number-of-bytes)
  (do-read-or-write primitive-read
		    platform-read fd pointer number-of-bytes))

(define (primitive-write fd pointer number-of-bytes)
  (do-read-or-write primitive-write
		    platform-write fd pointer number-of-bytes))

(define (primitive-pread fd pointer number-of-bytes offset)
  (do-pread-or-pwrite primitive-pread
		      platform-pread fd pointer number-of-bytes offset))

(define (primitive-pwrite fd pointer number-of-bytes offset)
  (do-pread-or-pwrite primitive-pwrite
		      platform-pwrite fd pointer number-of-bytes offset))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-read-function primitive-read)

(define-primitive-parameter
  primitive-write-function primitive-write)

(define-primitive-parameter
  primitive-pread-function primitive-pread)

(define-primitive-parameter
  primitive-pwrite-function primitive-pwrite)

;;; --------------------------------------------------------------------

(define (read fd pointer number-of-bytes)
  ((primitive-read-function) fd pointer number-of-bytes))

(define (write fd pointer number-of-bytes)
  ((primitive-write-function) fd pointer number-of-bytes))

(define (pread fd pointer number-of-bytes offset)
  ((primitive-pread-function) fd pointer number-of-bytes offset))

(define (pwrite fd pointer number-of-bytes offset)
  ((primitive-pwrite-function) fd pointer number-of-bytes offset))



;;;; seeking

(define (primitive-lseek fd offset whence)
  ;;It seems  that EINTR  cannot happen with  "lseek()", but it  does no
  ;;harm to use the macro.
  (temp-failure-retry-minus-one
   primitive-lseek
   (platform-lseek fd offset whence)
   fd))

(define-primitive-parameter
  primitive-lseek-function primitive-lseek)

(define (lseek fd offset whence)
  ((primitive-lseek-function) fd offset whence))



;;;; synchronisation

(define (primitive-sync)
  (receive (result errno)
      (platform-sync)
    (unless (= 0 result)
      (raise-errno-error 'primitive-sync errno #f))
    result))

(define (primitive-fsync fd)
  (call-for-minus-one primitive-fsync platform-fsync fd))

(define (primitive-fdatasync fd)
  (call-for-minus-one primitive-fdatasync platform-fdatasync fd))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-sync-function primitive-sync)

(define-primitive-parameter
  primitive-fsync-function primitive-fsync)

(define-primitive-parameter
  primitive-fdatasync-function primitive-fdatasync)

;;; --------------------------------------------------------------------

(define (sync)
  ((primitive-sync-function)))

(define (fsync fd)
  ((primitive-fsync-function) fd))

(define (fdatasync fd)
  ((primitive-fdatasync-function) fd))



;;;; control operations

(define (primitive-fcntl fd operation arg)
  (call-for-minus-one primitive-fcntl platform-fcntl fd operation arg))

(define (primitive-ioctl fd operation arg)
  (call-for-minus-one primitive-ioctl platform-ioctl fd operation arg))

(define-primitive-parameter
  primitive-fcntl-function primitive-fcntl)

(define-primitive-parameter
  primitive-ioctl-function primitive-ioctl)

(define (fcntl fd operation arg)
  ((primitive-fcntl-function) fd operation arg))

(define (ioctl fd operation arg)
  ((primitive-ioctl-function) fd operation arg))



;;;; duplicating

(define (primitive-dup fd)
  (call-for-minus-one primitive-dup platform-dup fd))

(define (primitive-dup2 old new)
  (call-for-minus-one primitive-dup2 platform-dup2 old new))

(define-primitive-parameter
  primitive-dup-function primitive-dup)

(define-primitive-parameter
  primitive-dup2-function primitive-dup2)

(define (dup fd)
  ((primitive-dup-function) fd))

(define (dup2 old new)
  ((primitive-dup2-function) old new))



;;;; making pipes

(define (primitive-pipe)
  (with-compensations
    (let ((p (malloc-block/c (sizeof-int-array 2))))
      (receive (result errno)
	  (platform-pipe p)
	(if (= -1 result)
	    (raise-errno-error 'primitive-pipe errno)
	  (values (array-ref-c-signed-int p 0)
		  (array-ref-c-signed-int p 1)))))))

(define (primitive-mkfifo pathname mode)
  (with-compensations
    (let ((c-pathname (string->cstring/c pathname)))
      (receive (result errno)
	  (platform-mkfifo c-pathname mode)
	(if (= -1 result)
	    (raise-errno-error 'primitive-mkfifo errno
			       (list pathname mode))
	  result)))))

(define-primitive-parameter
  primitive-pipe-function primitive-pipe)

(define-primitive-parameter
  primitive-mkfifo-function primitive-mkfifo)

(define (pipe)
  ((primitive-pipe-function)))

(define (mkfifo pathname mode)
  ((primitive-mkfifo-function) pathname mode))



;;;; custom binary ports

(define (custom-binary-read fd bv start count)
;;;  (format #t "reading ~s bytes from custom binary port~%" count)
  (with-compensations
    (let* ((p	(malloc-block/c count))
	   (len	(read fd p count)))
;;;      (format #t "actually read ~s bytes~%" len)
      (do ((i 0 (+ 1 i)))
	  ((= i len)
;;;	   (format #t "done~%")
	   len)
	(bytevector-u8-set! bv (+ start i) (pointer-ref-c-unsigned-char p i))))))

(define (custom-binary-write fd bv start count)
  (with-compensations
    (let* ((p	(malloc-block/c count)))
      (do ((i 0 (+ 1 i)))
	  ((= i count)
	   (write fd p count))
	(pointer-set-c-signed-char! p i (bytevector-u8-ref bv (+ start i)))))))


(define (fd->binary-input-port fd)
  (make-custom-binary-input-port
   "fd input port"
   (lambda (bv start count) (custom-binary-read fd bv start count))
   (lambda () (lseek fd 0 SEEK_CUR))
   (lambda (pos) (lseek fd pos SEEK_SET))
   (lambda () (close fd))))

(define (fd->binary-output-port fd)
  (make-custom-binary-output-port
   "fd output port"
   (lambda (bv start count) (custom-binary-write fd bv start count))
   (lambda () (lseek fd 0 SEEK_CUR))
   (lambda (pos) (lseek fd pos SEEK_SET))
   (lambda () (close fd))))

(define (fd->binary-input/ouput-port fd)
  (make-custom-binary-input-port
   "fd input/output port"
   (lambda (bv start count) (custom-binary-read fd bv start count))
   (lambda (bv start count) (custom-binary-write fd bv start count))
   (lambda () (lseek fd 0 SEEK_CUR))
   (lambda (pos) (lseek fd pos SEEK_SET))
   (lambda () (close fd))))



;;;; custom textual ports

(define (custom-textual-read fd str start count)
;;;     (format #t "reading ~s bytes~%" count)
  (with-compensations
    (let* ((p	(malloc-block/c count))
	   (len	(read fd p count)))
;;;	 (format #t "actually read ~s bytes~%" len)
      (do ((i 0 (+ 1 i)))
	  ((= i len)
;;;	      (format #t "done~%")
	   len)
	(string-set! str (+ start i)
		     (integer->char (pointer-ref-c-unsigned-char p i)))))))

(define (custom-textual-write fd str start count)
  (with-compensations
    (let* ((p	(malloc-block/c count)))
      (do ((i 0 (+ 1 i)))
	  ((= i count)
	   (write fd p count))
	(pointer-set-c-signed-char! p i (char->integer (string-ref str (+ start i))))))))


(define (fd->textual-input-port fd)
  (make-custom-textual-input-port
   "fd input port"
   (lambda (str start count) (custom-textual-read fd str start count))
   (lambda () (lseek fd 0 SEEK_CUR))
   (lambda (pos) (lseek fd pos SEEK_SET))
   (lambda () (close fd))))

(define (fd->textual-output-port fd)
  (make-custom-textual-output-port
   "fd output port"
   (lambda (str start count) (custom-textual-write fd str start count))
   (lambda () (lseek fd 0 SEEK_CUR))
   (lambda (pos) (lseek fd pos SEEK_SET))
   (lambda () (close fd))))

(define (fd->textual-input/ouput-port fd)
  (make-custom-textual-input-port
   "fd input/output port"
   (lambda (str start count) (custom-textual-read fd str start count))
   (lambda (str start count) (custom-textual-write fd str start count))
   (lambda () (lseek fd 0 SEEK_CUR))
   (lambda (pos) (lseek fd pos SEEK_SET))
   (lambda ()
     (close fd))))


;;;; pipe ports

(define (pipe-binary-ports)
  (let-values (((in ou) (pipe)))
    (values (make-custom-binary-input-port
	     "fd pipe binary reading port"
	     (lambda (bv start count) (custom-binary-read in bv start count))
	     #f
	     #f
	     (lambda () (close in)))
	    (make-custom-binary-output-port
	     "fd pipe binary writing port"
	     (lambda (bv start count) (custom-binary-write ou bv start count))
	     #f
	     #f
	     (lambda () (close ou))))))

(define (pipe-textual-ports)
  (let-values (((in ou) (pipe)))
    (values (make-custom-textual-input-port
	     "fd pipe textual reading port"
	     (lambda (bv start count) (custom-textual-read in bv start count))
	     #f
	     #f
	     (lambda () (close in)))
	    (make-custom-textual-output-port
	     "fd pipe textual writing port"
	     (lambda (bv start count) (custom-textual-write ou bv start count))
	     #f
	     #f
	     (lambda () (close ou))))))



;;;; done

)

;;; end of file

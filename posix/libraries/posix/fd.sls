;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to the file descriptor libraries
;;;Date: Fri Dec  5, 2008
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
    open primitive-open primitive-open-function platform-open
    close primitive-close primitive-close-function platform-close

    read primitive-read primitive-read-function platform-read
    write primitive-write primitive-write-function platform-write
    pread primitive-pread primitive-pread-function platform-pread
    pwrite primitive-pwrite primitive-pwrite-function platform-pwrite

    lseek primitive-lseek primitive-lseek-function platform-lseek

    sync primitive-sync primitive-sync-function platform-sync
    fsync primitive-fsync primitive-fsync-function platform-fsync
    fdatasync primitive-fdatasync primitive-fdatasync-function platform-fdatasync

    fcntl primitive-fcntl primitive-fcntl-function platform-fcntl
    ioctl primitive-ioctl primitive-ioctl-function platform-ioctl

    dup primitive-dup primitive-dup-function platform-dup
    dup2 primitive-dup2 primitive-dup2-function platform-dup2

    pipe primitive-pipe-function primitive-pipe platform-pipe
    mkfifo primitive-mkfifo-function primitive-mkfifo platform-mkfifo

    fd->binary-input-port		fd->textual-input-port
    fd->binary-output-port		fd->textual-output-port
    fd->binary-input/ouput-port		fd->textual-input/ouput-port

    pipe-binary-ports			pipe-textual-ports)
  (import (except (r6rs) read write)
    (uriel lang)
    (uriel foreign)
    (posix sizeof)
    (rnrs mutable-strings (6)))

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

(define-c-function/with-errno platform-open
  (int open (char* int mode_t)))

(define (primitive-open pathname open-mode permissions)
  (with-compensations
    (let ((c-pathname (string->cstring/c pathname)))
      (temp-failure-retry-minus-one
       primitive-open
       (platform-open c-pathname open-mode permissions)
       (list pathname open-mode permissions)))))

(define primitive-open-function
  (make-parameter primitive-open
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-open-function
	  "expected procedure as value for PRIMITIVE-OPEN-FUNCTION parameter"
	  func))
      func)))

(define (open pathname open-mode permissions)
  ((primitive-open-function) pathname open-mode permissions))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-close
  (int close (int)))

(define (primitive-close fd)
  (temp-failure-retry-minus-one
   primitive-close
   (platform-close fd)
   fd))

(define primitive-close-function
  (make-parameter primitive-close
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-close-function
	  "expected procedure as value for PRIMITIVE-CLOSE-FUNCTION parameter"
	  func))
      func)))

(define (close fd)
  ((primitive-close-function) fd))



;;;; reading and writing

(define-c-function/with-errno platform-read
  (ssize_t read (int void* size_t)))

(define-c-function/with-errno platform-pread
  (ssize_t pread (int void* size_t off_t)))

(define-c-function/with-errno platform-write
  (ssize_t write (int void* size_t)))

(define-c-function/with-errno platform-pwrite
  (ssize_t pwrite (int void* size_t off_t)))

;;; --------------------------------------------------------------------

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

(define (primitive-read fd pointer number-of-bytes)
  (do-read-or-write 'primitive-read
		    platform-read fd pointer number-of-bytes))

(define (primitive-write fd pointer number-of-bytes)
  (do-read-or-write 'primitive-write
		    platform-write fd pointer number-of-bytes))

(define (primitive-pread fd pointer number-of-bytes offset)
  (do-pread-or-pwrite 'primitive-pread
		      platform-pread fd pointer number-of-bytes offset))

(define (primitive-pwrite fd pointer number-of-bytes offset)
  (do-pread-or-pwrite 'primitive-pwrite
		      platform-pwrite fd pointer number-of-bytes offset))

;;; --------------------------------------------------------------------

(define primitive-read-function
  (make-parameter primitive-read
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-read-function
	  "expected procedure as value for PRIMITIVE-READ-FUNCTION parameter"
	  func))
      func)))

(define primitive-write-function
  (make-parameter primitive-write
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-write-function
	  "expected procedure as value for PRIMITIVE-WRITE-FUNCTION parameter"
	  func))
      func)))

(define primitive-pread-function
  (make-parameter primitive-pread
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-pread-function
	  "expected procedure as value for PRIMITIVE-PREAD-FUNCTION parameter"
	  func))
      func)))

(define primitive-pwrite-function
  (make-parameter primitive-pwrite
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-pwrite-function
	  "expected procedure as value for PRIMITIVE-PWRITE-FUNCTION parameter"
	  func))
      func)))

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

(define-c-function/with-errno platform-lseek
  (off_t lseek (int off_t int)))


(define (primitive-lseek fd offset whence)
  ;;It seems  that EINTR  cannot happen with  "lseek()", but it  does no
  ;;harm to use the macro.
  (temp-failure-retry-minus-one
   primitive-lseek
   (platform-lseek fd offset whence)
   fd))

(define primitive-lseek-function
  (make-parameter primitive-lseek
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-lseek-function
	  "expected procedure as value for PRIMITIVE-LSEEK-FUNCTION parameter"
	  func))
      func)))

(define (lseek fd offset whence)
  ((primitive-lseek-function) fd offset whence))



;;;; synchronisation

(define-c-function/with-errno platform-sync
  (int sync (void)))

(define-c-function/with-errno platform-fsync
  (int fsync (int)))

(define-c-function/with-errno platform-fdatasync
  (int fdatasync (int)))

;;; --------------------------------------------------------------------

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

(define primitive-sync-function
  (make-parameter primitive-sync
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-sync-function
	  "expected procedure as value for PRIMITIVE-SYNC-FUNCTION parameter"
	  func))
      func)))

(define primitive-fsync-function
  (make-parameter primitive-fsync
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fsync-function
	  "expected procedure as value for PRIMITIVE-FSYNC-FUNCTION parameter"
	  func))
      func)))

(define primitive-fdatasync-function
  (make-parameter primitive-fdatasync
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fdatasync-function
	  "expected procedure as value for PRIMITIVE-FDATASYNC-FUNCTION parameter"
	  func))
      func)))

;;; --------------------------------------------------------------------

(define (sync)
  ((primitive-sync-function)))

(define (fsync fd)
  ((primitive-fsync-function) fd))

(define (fdatasync fd)
  ((primitive-fdatasync-function) fd))



;;;; control operations

(define-c-function/with-errno platform-fcntl
  (int fcntl (int int int)))

(define-c-function/with-errno platform-ioctl
  (int ioctl (int int int)))

(define (primitive-fcntl fd operation arg)
  (call-for-minus-one primitive-fcntl platform-fcntl fd operation arg))

(define (primitive-ioctl fd operation arg)
  (call-for-minus-one primitive-ioctl platform-ioctl fd operation arg))

(define primitive-fcntl-function
  (make-parameter primitive-fcntl
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fcntl-function
	  "expected procedure as value for PRIMITIVE-FCNTL-FUNCTION parameter"
	  func))
      func)))

(define primitive-ioctl-function
  (make-parameter primitive-ioctl
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-ioctl-function
	  "expected procedure as value for PRIMITIVE-IOCTL-FUNCTION parameter"
	  func))
      func)))

(define (fcntl fd operation arg)
  ((primitive-fcntl-function) fd operation arg))

(define (ioctl fd operation arg)
  ((primitive-ioctl-function) fd operation arg))



;;;; duplicating

(define-c-function/with-errno platform-dup
  (int dup (int)))

(define-c-function/with-errno platform-dup2
  (int dup2 (int int)))

(define (primitive-dup fd)
  (call-for-minus-one primitive-dup platform-dup fd))

(define (primitive-dup2 old new)
  (call-for-minus-one primitive-dup2 platform-dup2 old new))

(define primitive-dup-function
  (make-parameter primitive-dup
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-dup-function
	  "expected procedure as value for PRIMITIVE-DUP-FUNCTION parameter"
	  func))
      func)))

(define primitive-dup2-function
  (make-parameter primitive-dup2
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-dup2-function
	  "expected procedure as value for PRIMITIVE-DUP2-FUNCTION parameter"
	  func))
      func)))

(define (dup fd)
  ((primitive-dup-function) fd))

(define (dup2 old new)
  ((primitive-dup2-function) old new))



;;;; making pipes

(define-c-function/with-errno platform-pipe
  (int pipe (pointer)))

(define (primitive-pipe)
  (with-compensations
    (let ((p (malloc-block/c (sizeof-int-array 2))))
      (receive (result errno)
	  (platform-pipe p)
	(if (= -1 result)
	    (raise-errno-error 'primitive-pipe errno)
	  (values (peek-array-signed-int p 0)
		  (peek-array-signed-int p 1)))))))

(define primitive-pipe-function
  (make-parameter primitive-pipe
    (lambda (func)
      (if (procedure? func)
	  func
	(assertion-violation 'primitive-pipe-function
	  "expected procedure as value for the PRIMITIVE-PIPE-FUNCTION parameter"
	  func)))))

(define (pipe)
  ((primitive-pipe-function)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-mkfifo
  (int mkfifo (char* mode_t)))

(define (primitive-mkfifo pathname mode)
  (with-compensations
    (let ((c-pathname (string->cstring/c pathname)))
      (receive (result errno)
	  (platform-mkfifo c-pathname mode)
	(if (= -1 result)
	    (raise-errno-error 'primitive-mkfifo errno
			       (list pathname mode))
	  result)))))

(define primitive-mkfifo-function
  (make-parameter primitive-mkfifo
    (lambda (func)
      (if (procedure? func)
	  func
	(assertion-violation 'primitive-mkfifo-function
	  "expected procedure as value for the PRIMITIVE-MKFIFO-FUNCTION parameter"
	  func)))))

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
	(bytevector-u8-set! bv (+ start i) (peek-unsigned-char p i))))))

(define (custom-binary-write fd bv start count)
  (with-compensations
    (let* ((p	(malloc-block/c count)))
      (do ((i 0 (+ 1 i)))
	  ((= i count)
	   (write fd p count))
	(poke-char! p i (bytevector-u8-ref bv (+ start i)))))))


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
		     (integer->char (peek-unsigned-char p i)))))))

(define (custom-textual-write fd str start count)
  (with-compensations
    (let* ((p	(malloc-block/c count)))
      (do ((i 0 (+ 1 i)))
	  ((= i count)
	   (write fd p count))
	(poke-char! p i (char->integer (string-ref str (+ start i))))))))


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
	     (lambda () (close in))))))



;;;; done

)

;;; end of file

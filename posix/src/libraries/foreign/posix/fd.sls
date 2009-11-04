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


(library (foreign posix fd)
  (export
    open	open-function
    close	close-function

    read	read-function
    write	write-function
    pread	pread-function
    pwrite	pwrite-function

    lseek	lseek-function

    sync	sync-function
    fsync	fsync-function
    fdatasync	fdatasync-function

    fcntl	fcntl-function
    ioctl	ioctl-function

    dup		dup-function
    dup2	dup2-function

    pipe	pipe-function
    mkfifo	mkfifo-function

    fd->binary-input-port		fd->textual-input-port
    fd->binary-output-port		fd->textual-output-port
    fd->binary-input/ouput-port		fd->textual-input/ouput-port

    pipe-binary-ports			pipe-textual-ports)
  (import (except (rnrs) read write)
    (compensations)
    (rnrs mutable-strings)
    (only (foreign memory)
	  malloc-block/c)
    (foreign posix helpers)
    (only (foreign ffi peekers-and-pokers)
	  pointer-set-c-uint8!
	  pointer-ref-c-uint8)
    (prefix (foreign posix fd primitives) primitive:)
    (only (foreign posix sizeof)
	  SEEK_CUR SEEK_SET))


;; opening and closing

(define-parameterised open pathname open-mode permissions)
(define-parameterised close fd)

;; reading and writing

(define-parameterised read fd pointer number-of-bytes)
(define-parameterised write fd pointer number-of-bytes)
(define-parameterised pread fd pointer number-of-bytes offset)
(define-parameterised pwrite fd pointer number-of-bytes offset)

;; seeking

(define-parameterised lseek fd offset whence)

;; synchronisation

(define-parameterised sync)
(define-parameterised fsync fd)
(define-parameterised fdatasync fd)

;; control operations

(define-parameterised fcntl fd operation arg)
(define-parameterised ioctl fd operation arg)

;; duplicating

(define-parameterised dup fd)
(define-parameterised dup2 old new)

;; making pipes

(define-parameterised pipe)
(define-parameterised mkfifo pathname mode)


;;;; custom binary ports

(define (custom-binary-read fd bv start count)
  (with-compensations
    (let* ((p	(malloc-block/c count))
	   (len	(read fd p count)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   len)
	(bytevector-u8-set! bv (+ start i) (pointer-ref-c-uint8 p i))))))

(define (custom-binary-write fd bv start count)
  (with-compensations
    (let* ((p	(malloc-block/c count)))
      (do ((i 0 (+ 1 i)))
	  ((= i count)
	   (write fd p count))
	(pointer-set-c-uint8! p i (bytevector-u8-ref bv (+ start i)))))))

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
  (with-compensations
    (let* ((p	(malloc-block/c count))
	   (len	(read fd p count)))
      (do ((i 0 (+ 1 i)))
	  ((= i len)
	   len)
	(string-set! str (+ start i)
		     (integer->char (pointer-ref-c-uint8 p i)))))))

(define (custom-textual-write fd str start count)
  (with-compensations
    (let* ((p	(malloc-block/c count)))
      (do ((i 0 (+ 1 i)))
	  ((= i count)
	   (write fd p count))
	(pointer-set-c-uint8! p i (char->integer (string-ref str (+ start i))))))))


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

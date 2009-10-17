;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: platform interface to POSIX fd functions
;;;Date: Thu Jan  1, 2009
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


#!r6rs
(library (posix fd platform)
  (export
    platform-open	platform-close
    platform-read	platform-write
    platform-pread	platform-pwrite
    platform-lseek
    platform-sync	platform-fsync
    platform-fdatasync
    platform-fcntl	platform-ioctl
    platform-dup	platform-dup2
    platform-pipe	platform-mkfifo
    )
  (import (rnrs)
    (foreign ffi)
    (posix sizeof))


;;;; code

(define-c-function/with-errno platform-open
  (int open (char* int mode_t)))

(define-c-function/with-errno platform-close
  (int close (int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-read
  (ssize_t read (int void* size_t)))

(define-c-function/with-errno platform-pread
  (ssize_t pread (int void* size_t off_t)))

(define-c-function/with-errno platform-write
  (ssize_t write (int void* size_t)))

(define-c-function/with-errno platform-pwrite
  (ssize_t pwrite (int void* size_t off_t)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-lseek
  (off_t lseek (int off_t int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-sync
  (int sync (void)))

(define-c-function/with-errno platform-fsync
  (int fsync (int)))

(define-c-function/with-errno platform-fdatasync
  (int fdatasync (int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-fcntl
  (int fcntl (int int int)))

(define-c-function/with-errno platform-ioctl
  (int ioctl (int int int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-dup
  (int dup (int)))

(define-c-function/with-errno platform-dup2
  (int dup2 (int int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-pipe
  (int pipe (pointer)))

(define-c-function/with-errno platform-mkfifo
  (int mkfifo (char* mode_t)))





;;;; done

)

;;; end of file

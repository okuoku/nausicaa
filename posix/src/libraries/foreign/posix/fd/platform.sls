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


(library (foreign posix fd platform)
  (export
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
    (parameters)
    (foreign posix shared-object)
    (foreign ffi)
    (foreign posix sizeof))

  (define dummy
    (shared-object standard-c-library))


(define-c-function/with-errno open
  (int open (char* int mode_t)))

(define-c-function/with-errno close
  (int close (int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno read
  (ssize_t read (int void* size_t)))

(define pread
  (parametrise ((shared-object libnausicaa-posix))
    (make-c-function/with-errno ssize_t nausicaa_posix_pread (int void* size_t off_t))))

(define-c-function/with-errno write
  (ssize_t write (int void* size_t)))

(define pwrite
  (parametrise ((shared-object libnausicaa-posix))
    (make-c-function/with-errno ssize_t nausicaa_posix_pwrite (int void* size_t off_t))))

;;; --------------------------------------------------------------------

(define lseek
  (parametrise ((shared-object libnausicaa-posix))
    (make-c-function/with-errno off_t nausicaa_posix_lseek (int off_t int))))

;;; --------------------------------------------------------------------

(define-c-function/with-errno sync
  (int sync (void)))

(define-c-function/with-errno fsync
  (int fsync (int)))

(define-c-function/with-errno fdatasync
  (int fdatasync (int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno fcntl
  (int fcntl (int int int)))

(define-c-function/with-errno ioctl
  (int ioctl (int int int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno dup
  (int dup (int)))

(define-c-function/with-errno dup2
  (int dup2 (int int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno pipe
  (int pipe (pointer)))

(define-c-function/with-errno mkfifo
  (int mkfifo (char* mode_t)))


;;;; done

)

;;; end of file

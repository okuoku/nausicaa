;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to platform functions that handle files
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


(library (foreign glibc file platform)
  (export

    ;; times
    lutimes		futimes

    ;; links
    alphasort		versionsort
    mknod

    ;; temporary files
    mktemp		mkstemp		mkdtemp
    tempnam		tmpnam		tmpfile)
  (import (except (rnrs)
		  remove truncate)
    (parameters)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign posix shared-object)
    (foreign posix sizeof))


(define dummy
  (shared-object standard-c-library))

;;; --------------------------------------------------------------------

(define-c-function/with-errno lutimes
  (int lutimes (char* pointer)))

(define-c-function/with-errno futimes
  (int futimes (int pointer)))


;;; --------------------------------------------------------------------

(define-c-function/with-errno truncate
  (int truncate (char* off_t)))

;;; --------------------------------------------------------------------
;;; special directory sort functions

(define-c-function alphasort
  (int alphasort (pointer pointer)))

(define-c-function versionsort
  (int versionsort (pointer pointer)))

;;; --------------------------------------------------------------------
;;; temporary files

(define-c-function/with-errno mktemp
  (char* mktemp (char*)))

(define-c-function/with-errno mkstemp
  (int mkstemp (char*)))

(define-c-function/with-errno mkdtemp
  (char* mkdtemp (char*)))

(define-c-function/with-errno tmpfile
  (FILE* tmpfile (void)))

(define-c-function/with-errno tempnam
  (char* tempnam (char* char*)))

(define-c-function/with-errno tmpnam
  (char* tmpnam (char*)))

;;; --------------------------------------------------------------------

(define mknod
  (parametrise ((shared-object libnausicaa-posix))
    (make-c-function/with-errno int nausicaa_posix_mknod (char* int int))))


;;;; done

)

;;; end of file

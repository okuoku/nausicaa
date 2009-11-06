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
    utimes		lutimes		futimes
    canonicalize_file_name
    remove
    truncate
    alphasort		versionsort
    ftw			nftw
    mknod
    tempnam		tmpnam		tmpnam_r
    tmpfile		mkdtemp)
  (import (except (rnrs)
		  remove truncate)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign posix sizeof))


(define dummy
  (shared-object self-shared-object))

;;; --------------------------------------------------------------------

(define-c-function/with-errno utimes
  (int utimes (char* pointer)))

(define-c-function/with-errno lutimes
  (int lutimes (char* pointer)))

(define-c-function/with-errno futimes
  (int futimes (int pointer)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno canonicalize_file_name
  (char* canonicalize_file_name (char*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno remove
  (int remove (char*)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno truncate
  (int truncate (char* off_t)))

;;; --------------------------------------------------------------------

(define-c-function alphasort
  (int alphasort (pointer pointer)))

(define-c-function versionsort
  (int versionsort (pointer pointer)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno ftw
  (int ftw (char* callback int)))

(define-c-function/with-errno nftw
  (int nftw (char* callback int int)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno tmpfile
  (FILE* tmpfile (void)))

(define-c-function/with-errno tempnam
  (char* tempnam (char*)))

(define-c-function/with-errno tmpnam
  (char* tmpnam (char*)))

(define-c-function/with-errno tmpnam_r
  (char* tmpnam_r (char*)))

(define-c-function/with-errno mkdtemp
  (char* mkdtemp (char*)))


(define dummy2
  (shared-object (open-shared-object* 'libnausicaa-posix1.so)))

(define-c-function/with-errno mknod
  (int nausicaa_posix_mknod (char* int int)))


;;;; done

)

;;; end of file

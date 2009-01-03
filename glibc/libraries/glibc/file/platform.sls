;;;
;;;Part of: Nausicaa/Glibc
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


;;;; setup

(library (glibc file platform)
  (export
    platform-canonicalize_file_name
    platform-remove
    platform-truncate
    platform-mkdtemp
    platform-alphasort
    platform-versionsort
    platform-ftw
    platform-nftw
    platform-mknod
    platform-tmpfile
    platform-mkdtemp
    platform-getumask)
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (glibc sizeof))

  (define d (shared-object self-shared-object))


;;;; code

(define-c-function/with-errno platform-canonicalize_file_name
  (char* canonicalize_file_name (char*)))

(define-c-function/with-errno platform-remove
  (int remove (char*)))

(define-c-function/with-errno platform-truncate
  (int truncate (char* off_t)))

(define-c-function/with-errno platform-mkdtemp
  (char* mkdtemp (char*)))

(define-c-function platform-alphasort
  (int alphasort (pointer pointer)))

(define-c-function platform-versionsort
  (int versionsort (pointer pointer)))

(define-c-function/with-errno platform-ftw
  (int ftw (char* callback int)))

(define-c-function/with-errno platform-nftw
  (int nftw (char* callback int int)))

(define-c-function/with-errno platform-mknod
  (int mknod (char* int int)))

(define-c-function/with-errno platform-tmpfile
  (FILE* tmpfile (void)))

(define-c-function/with-errno platform-mkdtemp
  (char* mkdtemp (char*)))

(define-c-function/with-errno platform-getumask
  (int getumask (void)))



;;;; done

)

;;; end of file

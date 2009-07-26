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


#!r6rs
(library (posix file platform)
  (export
     platform-getcwd
    platform-chdir
    platform-fchdir
    platform-opendir
    platform-fdopendir
    platform-dirfd
    platform-readdir
    platform-closedir
    platform-rewinddir
    platform-telldir
    platform-seekdir
    platform-scandir
    platform-link
    platform-symlink
    platform-readlink
    platform-realpath
    platform-unlink
    platform-rmdir
    platform-remove
    platform-rename
    platform-mkdir
    platform-chown
    platform-fchown
    platform-umask
    platform-chmod
    platform-fchmod
    platform-access
    platform-utime
    platform-utimes
    platform-lutimes
    platform-futimes
    platform-ftruncate
    platform-tmpnam
    platform-mktemp
    platform-mkstemp)
  (import (rnrs)
    (foreign ffi)
    (posix sizeof))

  (define dummy
    (shared-object self-shared-object))


;;;; code

(define-c-function/with-errno platform-getcwd
  (char* getcwd (char* size_t)))

(define-c-function/with-errno platform-chdir
  (int chdir (char*)))

(define-c-function/with-errno platform-fchdir
  (int fchdir (int)))

(define-c-function/with-errno platform-opendir
  (pointer opendir (char*)))

(define-c-function/with-errno platform-fdopendir
  (pointer fdopendir (int)))

(define-c-function/with-errno platform-dirfd
  (int dirfd (pointer)))

(define-c-function/with-errno platform-readdir
  (pointer readdir (pointer)))

(define-c-function/with-errno platform-closedir
  (int closedir (pointer)))

(define-c-function platform-rewinddir
  (void rewinddir (pointer)))

(define-c-function platform-telldir
  (long telldir (pointer)))

(define-c-function platform-seekdir
  (void seekdir (pointer long)))

(define-c-function/with-errno platform-scandir
  (int scandir (char* pointer callback callback)))

(define-c-function/with-errno platform-link
  (int link (char* char*)))

(define-c-function/with-errno platform-symlink
  (int symlink (char* char*)))

(define-c-function/with-errno platform-readlink
  (int readlink (char* char* size_t)))

(define-c-function/with-errno platform-realpath
  (char* realpath (char* char*)))

(define-c-function/with-errno platform-unlink
  (int unlink (char*)))

(define-c-function/with-errno platform-rmdir
  (int rmdir (char*)))

(define-c-function/with-errno platform-remove
  (int remove (char*)))

(define-c-function/with-errno platform-rename
  (int rename (char* char*)))

(define-c-function/with-errno platform-mkdir
  (int mkdir (char* mode_t)))

(define-c-function/with-errno platform-chown
  (int chown (char* uid_t gid_t)))

(define-c-function/with-errno platform-fchown
  (int fchown (int int int)))

(define-c-function/with-errno platform-umask
  (mode_t umask (mode_t)))

(define-c-function/with-errno platform-chmod
  (int chmod (char* mode_t)))

(define-c-function/with-errno platform-fchmod
  (int fchmod (int int)))

(define-c-function/with-errno platform-access
  (int access (char* int)))

(define-c-function/with-errno platform-utime
  (int utime (char* pointer)))

(define-c-function/with-errno platform-utimes
  (int utimes (char* pointer)))

(define-c-function/with-errno platform-lutimes
  (int lutimes (char* pointer)))

(define-c-function/with-errno platform-futimes
  (int futimes (int pointer)))

(define-c-function/with-errno platform-ftruncate
  (int ftruncate (int off_t)))

(define-c-function platform-tmpnam
  (char* tmpnam (char*)))

(define-c-function/with-errno platform-mktemp
  (char* mktemp (char*)))

(define-c-function/with-errno platform-mkstemp
  (int mkstemp (char*)))


;;;; done

)

;;; end of file

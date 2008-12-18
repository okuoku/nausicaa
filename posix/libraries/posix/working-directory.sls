;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to POSIX functions for R6RS Scheme
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-12-18 21:19:19 marco>
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

(library (posix working-directory)
  (export
    getcwd primitive-getcwd (rename (getcwd pwd))
    chdir primitive-chdir)
  (import (r6rs)
    (posix compat)
    (uriel lang)
    (uriel memory)
    (uriel cstring)
    (uriel ffi)
    (uriel errno)
    (srfi receive))

  (define dummy
    (shared-object self-shared-object))

  (define-c-function/with-errno primitive-getcwd
    (char* getcwd (char* size_t)))

  (define-c-function/with-errno primitive-chdir
    (int chdir (char*)))

  (define (getcwd)
    (let loop ((buflen 1024))
      (with-compensations
	(let ((buffer (malloc-block/c buflen)))
	  (receive (cstr errno)
	      (primitive-getcwd buffer buflen)
	    (if (and (= 0 (pointer->integer cstr))
		     (or (= EINVAL errno)
			 (= ERANGE errno)))
		(loop (* 2 buflen))
	      (begin
		(when (pointer-null? cstr)
		  (raise-errno-error 'getcwd errno #f))
		(cstring->string buffer))))))))

  (define (chdir directory-pathname)
    (with-compensations
      (receive (result errno)
	  (primitive-chdir (string->cstring/c directory-pathname))
	(unless (= 0 result)
	  (raise-errno-error 'chdir errno directory-pathname))
	result))))

;;; end of file

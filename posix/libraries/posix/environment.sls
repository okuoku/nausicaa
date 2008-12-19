;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to POSIX functions for R6RS Scheme
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-12-19 16:32:41 marco>
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

(library (posix environment)
  (export
    getenv setenv)
  (import (r6rs)
    (posix environment compat)
    (uriel lang)
    (uriel foreign))

  (define dummy
    (shared-object self-shared-object))

  (define-c-function primitive-setenv
    (int setenv (char* char* int)))

  (define (setenv varname newvalue replace)
    (with-compensations
      (letrec
	  ((name  (string->cstring/c varname))
	   (value (string->cstring/c newvalue)))
	(primitive-setenv name value (if replace 1 0)))))

  (define (getenv varname)
    ;;Currently   the   supported   Scheme   implementations   providing
    ;;PRIMITIVE-GETENV automatically return a Scheme string.
    ;;
    ;;PRIMITIVE-GETENV is supposed  to return #f is the  variable is not
    ;;set.
    (primitive-getenv (symbol->string/maybe varname)))

;;;To use  "unsetenv()" the  memory block must  be persistent  (read the
;;;documentation of the  GNU C library).  This is not  a good thing with
;;;garbage collection.
  ;;
  ;; (define-c-function unsetenv-stub
  ;;
  ;;   (int unsetenv (char*)))
  ;;
  ;; (define (unsetenv varname)
  ;;   (with-compensations
  ;;     (unsetenv-stub (string->cstring/c varname))))

  )

;;; end of file

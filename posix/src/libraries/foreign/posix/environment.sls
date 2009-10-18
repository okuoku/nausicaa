;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to POSIX functions for R6RS Scheme
;;;Date: Mon Nov 24, 2008
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


(library (foreign posix environment)
  (export getenv setenv)
  (import (rnrs)
    (only (foreign posix helpers)
	  define-primitive-parameter)
    (foreign posix environment primitives))

  (define-primitive-parameter setenv-function primitive-setenv)

  (define-primitive-parameter getenv-function primitive-getenv)

  (define setenv
    (case-lambda
     ((varname newvalue)
      (setenv varname newvalue #t))
     ((varname newvalue replace)
      ((setenv-function) varname newvalue replace))))

  (define (getenv varname)
    ((getenv-function) varname))

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

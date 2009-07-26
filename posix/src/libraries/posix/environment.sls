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


#!r6rs
(library (posix environment)
  (export
    getenv setenv)
  (import (nausicaa)
    (foreign)
    (posix sizeof)
    (posix environment platform))


;;;; setting and getting

(define (primitive-setenv varname newvalue replace)
  (with-compensations
    (letrec
	((name  (string->cstring/c varname))
	 (value (string->cstring/c newvalue)))
      (platform-setenv name value (if replace 1 0)))))

(define (primitive-getenv varname)
  (with-compensations
    (let ((p (platform-getenv (string->cstring/c varname))))
      (if (pointer-null? p)
	  #f
	(cstring->string p)))))

(define-primitive-parameter
  primitive-setenv-function primitive-setenv)

(define-primitive-parameter
  primitive-getenv-function primitive-getenv)

(define (setenv varname newvalue replace)
  ((primitive-setenv-function) varname newvalue replace))

(define (getenv varname)
  ((primitive-getenv-function) varname))

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


;;;; done

)

;;; end of file

;;;
;;;Part of: Uriel libraries
;;;Contents: interface to POSIX functions
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-12-03 10:06:47 marco>
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



;;;; setup

(library (uriel posix)
  (export

    ;;environment variables
    getenv setenv

    ;;working directory
    getcwd primitive-getcwd (rename (getcwd pwd))
    chdir primitive-chdir

    )
  (import (rnrs)
    (uriel lang)
    (uriel ffi)
    (uriel ffi errno)
    (uriel posix compat)
    (uriel printing)
    (srfi receive))



;;;; helpers




;;;; environment variables

(define-c-function primitive-setenv
  (int setenv (char* char* int)))

(define (setenv varname newvalue replace)
  (with-compensations
    (letrec
	((name (string-or-symbol->cstring/compensated varname))
	 (value (string-or-symbol->cstring/compensated newvalue)))
      (primitive-setenv name value (if replace 1 0)))))

(define (getenv varname)
  ;;Currently    the   supported   Scheme    implementations   providing
  ;;PRIMITIVE-GETENV automatically return a Scheme string.
  ;;
  ;;PRIMITIVE-GETENV is  supposed to  return #f is  the variable  is not
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
;;     (letrec
;; 	((name (compensate
;; 		   (string->cstring (symbol->string/maybe varname))
;; 		 (with
;; 		  (primitive-free name)))))
;;       (unsetenv-stub name))))



;;;;working directory

(define-c-function/with-errno primitive-getcwd
  (char* getcwd (char* size_t)))

(define-c-function/with-errno primitive-chdir
  (int chdir (char*)))

(define (getcwd)
  (let loop ((buflen 1024))
    (with-compensations
      (letrec*
	  ((buffer (compensate
		       (malloc buflen)
		     (with
		      (primitive-free buffer)))))
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
    (letrec
	((buffer (string-or-symbol->cstring/compensated directory-pathname)))
      (receive (result errno)
	  (primitive-chdir buffer)
	(unless (= 0 result)
	  (raise-errno-error 'chdir errno directory-pathname))
	result))))



;;;; done

)

;;; end of file

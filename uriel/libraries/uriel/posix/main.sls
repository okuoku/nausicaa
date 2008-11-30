;;;
;;;Part of: Uriel libraries
;;;Contents: interface to POSIX functions
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-11-30 18:12:35 marco>
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
    getcwd pwd chdir
    )
  (import (rnrs)
    (uriel lang)
    (uriel ffi)
    (uriel posix compat)
    (srfi receive))



;;;; helpers




;;;; environment variables

(define-c-function setenv-stub
  (int setenv (char* char* int)))

(define (setenv varname newvalue replace)
  (with-compensations
    (letrec
	((name (compensate
		   (string->cstring (symbol->string/maybe varname))
		 (with
		  (primitive-free name))))
	 (value (compensate
		    (string->cstring (symbol->string/maybe newvalue))
		  (with
		   (primitive-free value)))))
      (setenv-stub name value (if replace 1 0)))))

(define (getenv varname)
  ;;Currently the supported Scheme implementations providing GETENV-STUB
  ;;automatically return a Scheme string.
  ;;
  ;;GETENV-STUB is supposed to return #f is the variable is not set.
  (getenv-stub (symbol->string/maybe varname)))

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

(define-c-function/with-errno getcwd-stub
  (char* getcwd (char* size_t)))

(define (getcwd)
  (with-compensations
    (letrec*
	((buflen 4096) ; enough? or is there a limit of 1024?
	 (buffer (compensate
		     (malloc 4096)
		   (with
		    (primitive-free buffer)))))
      (receive (cstr errno)
	  (getcwd-stub buffer buflen)
	(values (cstring->string buffer)
		errno)))))

(define pwd getcwd)

(define-c-function/with-errno chdir-stub
  (int chdir (char*)))

(define (chdir directory-pathname)
  (with-compensations
    (letrec
	((buffer (compensate
		     (string->cstring
		      (symbol->string/maybe directory-pathname))
		   (with
		    (primitive-free buffer)))))
      (chdir-stub buffer))))



;;;; done

)

;;; end of file

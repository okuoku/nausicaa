;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility layer for Mosh FFI
;;;Date: Thu Jun 25, 2009
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


(library (foreign ffi compat)
  (export
    shared-object primitive-open-shared-object self-shared-object
    primitive-make-c-function primitive-make-c-function/with-errno
    errno)
  (import (rnrs)
    (only (system)
	  make-parameter)
    (rename (only (mosh ffi)
		  open-shared-library make-c-function
		  shared-errno)
	    (shared-errno errno)))


;;;; dynamic loading

(define self-shared-object
  (open-shared-library ""))

(define shared-object
  (make-parameter self-shared-object))

;;In case of error this raises an exception automatically.
(define primitive-open-shared-object open-shared-library)


;;;; values normalisation: Foreign -> Mosh
;;
;;According to "lib/mosh/ffi.ss" (Fri Jul 3, 2009):
;;
;;* The accepted return values are: void*, char*, int, double, void.
;;
;;* The accepted arguments are:     void*, char*, int, double.
;;

(define (nausicaa-type->mosh-type type)
  (case type
    ((void)
     'void)
    ((int
      signed-int ssize_t uint unsigned unsigned-int size_t
      long signed-long ulong unsigned-long)
     'int)
    ((double float)
     'double)
    ((pointer void* char* FILE*)
     'void*)
    ((callback)
     'void*)
    (else
     (assertion-violation 'make-c-function
       "unknown C language type identifier" type))))


;;;; interface functions

(define (primitive-make-c-function ret-type funcname arg-types)
  (make-c-function (shared-object)
		   (nausicaa-type->mosh-type ret-type)
		   funcname
		   (map nausicaa-type->mosh-type arg-types)))

(define (primitive-make-c-function/with-errno ret-type funcname arg-types)
  (let ((f (primitive-make-c-function ret-type funcname arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (errno 0)
			  (apply f args)))
	     (errval	(errno)))
	(values retval errval)))))


;;;; done

)

;;; end of file

;;;
;;;Part of: Uriel libraries
;;;Contents: FFI compatibility layer for Ypsilon
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-11-24 18:51:00 marco>
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

(library (uriel ffi compat)
  (export
    shared-object primitive-open-shared-object primitive-make-c-function
    primitive-malloc primitive-free
    strlen string->cstring cstring->string)
  (import (rnrs)
    (core)
    (srfi parameters)
    (ffi))


;;;; dynamic loading and interface functions

(define shared-object
  (make-parameter (load-shared-object "")))

(define libc-name "C library")

(define (primitive-open-shared-object library-name)
  ;;This raises an exception automatically.
  (load-shared-object library-name))

(define (external->internal type)
  (case type
    ((int signed-int uint unsigned unsigned-int size_t ssize_t)
     'int)
    ((double)
     'double)
    ((pointer void* char*)
     'void*)
    ((void)
     'void)
    (else (error 'make-c-function
	    "unknown C language type identifier" type))))

(define-syntax make-c-callout
  (lambda (macro-use-stx)
    (syntax-case macro-use-stx ()
      ((_ ?shared-object ?ret-type ?funcname (?arg-type ...))
       (syntax (list 'c-function ?shared-object "A Foreign Library"
		     (external->internal '?ret-type)
		     '__stdcall '?funcname
		     (map external->internal '(?arg-type ...))))))))

(define-syntax primitive-make-c-function
  (syntax-rules ()
    ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
     (make-c-callout (shared-object)
		     ?ret-type
		     ?funcname
		     (?arg-type0 ?arg-type ...)))))



;;;; memory allocation

(define platform-malloc
  (primitive-make-c-function pointer malloc (size_t)))
;;  (c-function (shared-object) libc-name char* malloc (int)))

(define (primitive-malloc number-of-bytes)
  (let ((p (platform-malloc number-of-bytes)))
    (if (= 0 p)
	#f
      p)))

(define primitive-free
  (primitive-make-c-function void free (pointer)))

;;  (c-function (shared-object) libc-name void free (char*)))


;;;; string functions

(define strlen
  (primitive-make-c-function int strlen (pointer)))

;;  (c-function (shared-object) libc-name int __stdcall strlen (char*)))

(define (cstring->string cstr)
  (let ((str (bytevector->string cstr (make-transcoder (utf-8-codec)))))
    (substring str 0 (- (string-length str) 1))))

(define (string->cstring str)
  (string->utf8 (string-append str "\x0;")))



;;;; done

)

;;; end of file

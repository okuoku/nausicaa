;;;
;;;Part of: Uriel libraries for Ikarus
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;Time-stamp: <2008-11-22 07:40:57 marco>
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


;;;; Setup.

(library (uriel ffi)
  (export
    uriel-cleanup malloc block-guardian
    make-c-callout
    dlopen dlsym dlclose
    strlen string->cstring cstring->string)
  (import (rnrs)
    (only (ikarus) make-guardian)
    (prefix (ikarus foreign) ike:))


;;;; Dynamic loading.

(define dlopen
  (case-lambda
   (()
    (let ((l (ike:dlopen)))
      (or l (error 'dlopen (ike:dlerror)))))
   ((libname)
    (let ((l (ike:dlopen (if (symbol? libname)
			     (symbol->string libname)
			   libname))))
      (or l (error 'dlopen (ike:dlerror)))))
   ((libname lazy? global?)
    (let ((l (ike:dlopen (if (symbol? libname)
			     (symbol->string libname)
			   libname)
			 lazy? global?)))
      (or l (error 'dlopen (ike:dlerror)))))))

(define (dlsym library funcname)
  (let ((f (ike:dlsym library (if (symbol? funcname)
				  (symbol->string funcname)
				funcname))))
    (or f (error 'dlsym (ike:dlerror) (cons library funcname)))))

(define (dlclose library)
  (unless (dlclose library)
    (error 'dlclose (ike:dlerror) library)))


;;;; Memory allocation.

(define block-guardian (make-guardian))

(define (uriel-cleanup)
  (do ((p (block-guardian) (block-guardian)))
      ((p))
    (ike:free p)))

(define (malloc size)
  (let ((p (ike:malloc size)))
    (unless p
      (error "memory allocation error"))
    (block-guardian p)
    p))



;;;; C wrappers.

(define (signature-hash signature)
  (abs (apply +
	      (symbol-hash (car signature))
	      (map symbol-hash (cadr signature)))))

(define *callout-table*
  (make-hashtable signature-hash equal?))

(define (make-c-callout-maybe spec)
  (let ((f (hashtable-ref *callout-table* spec #f)))
    (or f (let ((f (apply ike:make-c-callout spec)))
	    (hashtable-set! *callout-table* spec f)
	    f))))

(define-syntax make-c-callout
  (syntax-rules ()
    ((_ ?retval (?arg-type0 ?arg-type ...))
     (make-c-callout-maybe '(?retval (?arg-type0 ?arg-type ...))))))


;;;; String functions.

(define (strlen p)
  (let loop ((i 0))
    (if (= 0 (ike:pointer-ref-c-unsigned-char p i))
	i
      (loop (+ 1 i)))))

(define (string->cstring s)
  (let* ((len	(string-length s))
	 (p	(malloc (+ 1 len)))
	 (bv	(string->utf8 s)))
    (ike:pointer-set-c-char! p len 0)
    (do ((i 0 (+ 1 i)))
	((= i len)
	 p)
      (ike:pointer-set-c-char! p i (bytevector-s8-ref bv i)))))

(define (cstring->string p)
  (let* ((len	(strlen p))
	 (bv	(make-bytevector len)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (utf8->string bv))
      (bytevector-s8-set! bv i (ike:pointer-ref-c-signed-char p i)))))



) ;; end of library form

;;; end of file

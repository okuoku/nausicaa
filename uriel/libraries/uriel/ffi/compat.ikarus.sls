;;;
;;;Part of: Uriel libraries
;;;Contents: foreign functions interface compatibility layer for Ikarus
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-11-24 18:52:18 marco>
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
    (rename (malloc primitive-malloc) (free primitive-free))
    strlen string->cstring cstring->string)
  (import (rnrs)
    (uriel printing)
    (srfi parameters)
    (ikarus foreign))


;;;; dynamic loading and interface functions

(define shared-object
  (make-parameter (dlopen)))

(define (primitive-open-shared-object library-name)
  (let ((l (dlopen library-name)))
    (or l (error 'primitive-open-symbol-table
	    (dlerror) library-name))))

(define (external->internal type)
  (case type
    ((char signed-char)
     'signed-char)
    ((uchar unsigned-char)
     'unsigned-char)
    ((int signed-int ssize-t)
     'signed-int)
    ((uint unsigned unsigned-int size_t)
     'unsigned-int)
    ((long)
     'signed-long)
    ((ulong unsigned-long)
     'unsigned-long)
    ((float)
     'float)
    ((double)
     'double)
    ((pointer void* char*)
     'pointer)
    ((void)
     'void)
    (else (error 'make-c-function
	    "unknown C language type identifier" type))))

(define make-c-callout-maybe
  (letrec ((signature-hash
	    (lambda (signature)
	      (abs (apply +
			  (symbol-hash (car signature))
			  (map symbol-hash (cdr signature))))))
	   (callout-table
	    (make-hashtable signature-hash equal?)))
    (lambda (spec)
      (let* ((signature (map external->internal spec))
	     (f (hashtable-ref callout-table signature #f)))
	(or f (let ((f (make-c-callout (car signature) (cdr signature))))
		(hashtable-set! callout-table spec f)
		f))))))

(define (make-c-function funcname spec)
  (let ((f (dlsym (shared-object) (symbol->string funcname))))
    (unless f
      (error 'make-c-function (dlerror) funcname))
    ((make-c-callout-maybe spec) f)))

(define-syntax primitive-make-c-function
  (syntax-rules ()
    ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
     (make-c-function '?funcname '(?ret-type ?arg-type0 ?arg-type ...)))))


;;;; string functions

(define strlen
  (primitive-make-c-function unsigned strlen (pointer)))

;; (define (strlen p)
;;   (let loop ((i 0))
;;     (if (= 0 (pointer-ref-c-unsigned-char p i))
;; 	i
;;       (loop (+ 1 i)))))

(define (string->cstring s)
  (let* ((len	(string-length s))
	 (p	(malloc (+ 1 len)))
	 (bv	(string->utf8 s)))
    (pointer-set-c-char! p len 0)
    (do ((i 0 (+ 1 i)))
	((= i len)
	 p)
      (pointer-set-c-char! p i (bytevector-s8-ref bv i)))))

(define (cstring->string p)
  (let* ((len	(strlen p))
	 (bv	(make-bytevector len)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (utf8->string bv))
      (bytevector-s8-set! bv i (pointer-ref-c-signed-char p i)))))



;;;; done

)

;;; end of file

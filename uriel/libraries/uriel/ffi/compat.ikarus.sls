;;;
;;;Part of: Uriel libraries
;;;Contents: foreign functions interface compatibility layer for Ikarus
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-12-04 13:49:48 marco>
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

    ;;loading shared objects
    shared-object primitive-open-shared-object self-shared-object

    ;;interface functions
    primitive-make-c-function primitive-make-c-function/with-errno

    ;;basic memory allocation
    (rename (malloc primitive-malloc) (free primitive-free))

    ;;basic string conversion
    strlen string->cstring cstring->string strerror
    cstring->string/len

    ;;peekers
    pointer-ref-c-signed-char		pointer-ref-c-unsigned-char
    pointer-ref-c-signed-short		pointer-ref-c-unsigned-short
    pointer-ref-c-signed-int		pointer-ref-c-unsigned-int
    pointer-ref-c-signed-long		pointer-ref-c-unsigned-long
    pointer-ref-c-signed-long-long	pointer-ref-c-unsigned-long-long
    pointer-ref-c-float			pointer-ref-c-double
    pointer-ref-c-pointer

    ;;pokers
    pointer-set-c-char!			pointer-set-c-short!
    pointer-set-c-int!			pointer-set-c-long!
    pointer-set-c-long-long!		pointer-set-c-float!
    pointer-set-c-double!		pointer-set-c-pointer!

    ;;pointers
    pointer? pointer->integer integer->pointer pointer-null?

    ;;conditions
    raise-errno-error)
  (import (rnrs)
    (srfi parameters)
    (only (ikarus) strerror)
    (ikarus foreign)
    (uriel ffi errno)
    (uriel ffi conditions))



;;;; pointers

(define (pointer-null? pointer)
  (= 0 (pointer->integer pointer)))

;;;FIXME: temporary until the bug is fixed
(define pointer-set-c-long-long! #f)



;;;;  values normalisation: Uriel -> Ikarus

(define (external->internal type)
  (case type
    ((char schar signed-char)
     'signed-char)
    ((uchar unsigned-char)
     'unsigned-char)
    ((int signed-int ssize_t)
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
    ((pointer void* char* callback FILE*)
     'pointer)
    ((void)
     'void)
    (else (error 'make-c-function
	    "unknown C language type identifier" type))))



;;;; dynamic loading and interface functions

(define self-shared-object (dlopen))

(define shared-object
  (make-parameter self-shared-object))

(define (primitive-open-shared-object library-name)
  (let ((l (dlopen library-name)))
    (or l (error 'primitive-open-symbol-table
	    (dlerror) library-name))))

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
	(or f (let* ((f (make-c-callout (car signature)
					(if (equal? '(void) (cdr signature))
					    '()
					  (cdr signature)))))
		(hashtable-set! callout-table spec f)
		f))))))

(define (primitive-make-c-function ret-type funcname arg-types)
  (let ((f (dlsym (shared-object) (symbol->string funcname))))
    (unless f
      (error 'make-c-function (dlerror) funcname))
    ((make-c-callout-maybe (cons ret-type arg-types)) f)))

(define (primitive-make-c-function/with-errno ret-type funcname arg-types)
  (let ((f (primitive-make-c-function ret-type funcname arg-types)))
    (lambda args
      (values (apply f args) (errno)))))



;;;; string functions

(define strlen
  (primitive-make-c-function 'size_t 'strlen '(pointer)))

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

(define (cstring->string/len p len)
  (let* ((bv	(make-bytevector len)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (pointer-set-c-char! p i 0)
	 (utf8->string bv))
      (bytevector-s8-set! bv i (pointer-ref-c-signed-char p i)))))




;;;; conditions

;;This is not in "conditions.sls" because it requires STRERROR.
(define (raise-errno-error who errno irritants)
  (raise (condition (make-who-condition who)
		    (make-message-condition (strerror errno))
		    (make-errno-condition errno)
		    (make-irritants-condition irritants))))




;;;; done

)

;;; end of file

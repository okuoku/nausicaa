;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: foreign functions interface compatibility layer for Ikarus
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



;;;; setup

(library (foreign ffi compat)
  (export

    ;;loading shared objects
    shared-object primitive-open-shared-object self-shared-object

    ;;interface functions
    primitive-make-c-function primitive-make-c-function/with-errno
    primitive-make-c-callback

    errno)
  (import (ikarus)
    (rename (ikarus foreign)
	    (errno ikarus:errno)))


;;;;  values normalisation: Foreign -> Ikarus

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
    ((signed-long long)
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
	    (lambda (obj)
	      (let ((h (abs (apply + (map symbol-hash obj)))))
		h)))
	   (callout-table
	    (make-hashtable signature-hash equal?)))
    (lambda (spec)
      (let* ((signature (map external->internal spec))
	     (f (hashtable-ref callout-table signature #f)))
	(or f (let* ((f (make-c-callout (car signature)
					(if (equal? '(void) (cdr signature))
					    '()
					  (cdr signature)))))
		(hashtable-set! callout-table signature f)
		f))))))

(define (primitive-make-c-function ret-type funcname arg-types)
  (let ((f (dlsym (shared-object) (symbol->string funcname))))
    (unless f
      (error 'primitive-make-c-function (dlerror) funcname))
    ((make-c-callout-maybe (cons ret-type arg-types)) f)))

(define __errno_location
  (primitive-make-c-function 'pointer '__errno_location '(void)))

(define-syntax errno
  (syntax-rules ()
    ((_ ?value)
     (pointer-set-c-int! (__errno_location) 0 ?value))
    ((_)
     (ikarus:errno))))

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



;;;; callbacks

(define make-c-callback-maybe
  (letrec ((signature-hash
	    (lambda (obj)
	      (let ((h (abs (apply + (map symbol-hash obj)))))
		h)))
	   (callout-table
	    (make-hashtable signature-hash equal?)))
    (lambda (spec)
      (let* ((signature (map external->internal spec))
	     (f (hashtable-ref callout-table signature #f)))
	(or f (let* ((f (make-c-callout (car signature)
					(if (equal? '(void) (cdr signature))
					    '()
					  (cdr signature)))))
		(hashtable-set! callout-table signature f)
		f))))))

(define (primitive-make-c-callback scheme-function ret-type arg-types)
  ((make-c-callout-maybe (cons ret-type arg-types)) scheme-function))



;;;; done

)

;;; end of file

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


(library (foreign ffi compat)
  (export
    shared-object primitive-open-shared-object self-shared-object
    primitive-make-c-function primitive-make-c-function/with-errno
    primitive-make-c-callback primitive-free-c-callback
    errno)
  (import (rnrs)
    (only (ikarus)
	  make-parameter)
    (rename (only (ikarus foreign)
		  dlopen dlsym dlerror
		  make-c-callout make-c-callback
		  errno pointer-set-c-int!)
	    (errno ikarus:errno)))


;;;; types normalisation

(define (nausicaa-type->ikarus-type type)
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
  ;;Wrapper for DLOPEN that raises an exception if failure.
  ;;
  (let ((l (dlopen library-name)))
    (or l (error 'primitive-open-shared-object (dlerror) library-name))))

(define (primitive-make-c-function lib-spec ret-type funcname arg-types)
  (let ((f (dlsym lib-spec (symbol->string funcname))))
    (if f
	((%make-c-callout-maker (cons ret-type arg-types)) f)
      (error 'primitive-make-c-function (dlerror) funcname))))

(define %make-c-callout-maker
  ;;Given a  signature of types,  create a callout  closure constructor;
  ;;cache the closure constructors to avoid duplication.
  ;;
  ;;The signature is a list of  Scheme symbols, so it requires an ad hoc
  ;;hash function.  There are better hash functions than SIGNATURE-HASH,
  ;;but speed is also important.
  ;;
  (letrec ((signature-hash	(lambda (obj)
				  (abs (apply + (map symbol-hash obj)))))
	   (callout-maker-table	(make-hashtable signature-hash equal?)))
    (lambda (spec)
      (let* ((signature		(map nausicaa-type->ikarus-type spec))
	     (callout-maker	(hashtable-ref callout-maker-table signature #f)))
	(or callout-maker
	    (let* ((ret-type	(car signature))
		   (arg-types	(cdr signature))
		   (callout-maker (make-c-callout ret-type
						  (if (equal? '(void) arg-types)
						      '()
						    arg-types))))
	      (hashtable-set! callout-maker-table signature callout-maker)
	      callout-maker))))))

(define __errno_location
  (primitive-make-c-function self-shared-object 'pointer '__errno_location '(void)))

(define-syntax errno
  (syntax-rules ()
    ((_ ?value)
     (pointer-set-c-int! (__errno_location) 0 ?value))
    ((_)
     (ikarus:errno))))

(define (primitive-make-c-function/with-errno lib-spec ret-type funcname arg-types)
  (let ((callout-closure (primitive-make-c-function lib-spec ret-type funcname arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: We
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (errno 0)
			  (apply callout-closure args)))
	     (errval	(errno)))
	(values retval errval)))))


;;;; callbacks

(define (primitive-make-c-callback ret-type scheme-function arg-types)
  ((%make-c-callback-maker (cons ret-type arg-types)) scheme-function))

(define %make-c-callback-maker
  ;;Given a  signature of types, create a  callback closure constructor;
  ;;cache the closure constructors to avoid duplication.
  ;;
  ;;The signature is a list of  Scheme symbols, so it requires an ad hoc
  ;;hash function.  There are better hash functions than SIGNATURE-HASH,
  ;;but speed is also important.
  ;;
  (letrec ((signature-hash		(lambda (obj)
					  (abs (apply + (map symbol-hash obj)))))
	   (callback-maker-table	(make-hashtable signature-hash equal?)))
    (lambda (spec)
      (let* ((signature		(map nausicaa-type->ikarus-type spec))
	     (callback-maker	(hashtable-ref callback-maker-table signature #f)))
	(or callback-maker
	    (let* ((ret-type	(car signature))
		   (arg-types	(cdr signature))
		   (callback-maker (make-c-callback ret-type
						    (if (equal? '(void) arg-types)
							'()
						      arg-types))))
	      (hashtable-set! callback-maker-table signature callback-maker)
	      callback-maker))))))

(define (primitive-free-c-callback cb)
  #f)


;;;; done

)

;;; end of file

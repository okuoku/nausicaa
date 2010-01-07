;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: foreign functions interface compatibility layer for Ikarus
;;;Date: Mon Nov 24, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign ffi platform)
  (export
    open-shared-object		lookup-shared-object
    make-c-callout		make-c-callout/with-errno
    make-c-callback		free-c-callback)
  (import (rnrs)
    (only (foreign ffi sizeof)
	  LIBC_SHARED_OBJECT_SPEC)
    (foreign ffi conditions)
    (prefix (only (ikarus foreign)
		  dlopen dlsym dlerror
		  make-c-callout make-c-callback
		  errno pointer-set-c-int!)
	    ikarus:))


;;;; helpers

(define (%normalise-arg-types arg-types)
  (if (equal? '(void) arg-types)
      '()
    arg-types))

(define (%normalise-foreign-symbol foreign-symbol)
  (if (symbol? foreign-symbol)
      (symbol->string foreign-symbol)
    foreign-symbol))

(define (%signature-hash signature)
  (abs (apply + (map symbol-hash signature))))

(define callout-maker-table
  (make-hashtable %signature-hash equal?))


;;;; dynamic loading

(define (open-shared-object library-name)
  (ikarus:dlopen (%normalise-foreign-symbol library-name)))

(define (lookup-shared-object lib-spec foreign-symbol)
  ;;This already returns #f when the symbol is not found.
  (ikarus:dlsym lib-spec (%normalise-foreign-symbol foreign-symbol)))


;;;; errno interface

(define libc-shared-object
  (ikarus:dlopen LIBC_SHARED_OBJECT_SPEC))

(define errno-location
  ((ikarus:make-c-callout 'pointer '())
   (ikarus:dlsym libc-shared-object "__errno_location")))

(define-syntax errno
  (syntax-rules ()
    ((_ ?value)
     (ikarus:pointer-set-c-int! (errno-location) 0 ?value))
    ((_)
     (ikarus:errno))))


;;;; callout functions

;; (define (make-c-callout ret-type address arg-types)
;;   ((ikarus:make-c-callout ret-type (%normalise-arg-types arg-types)) address))
(define (make-c-callout ret-type address arg-types)
  ;;Given a  signature of types,  create a callout  closure constructor;
  ;;cache the closure constructors to avoid duplication.
  ;;
  ;;The signature is a list of  Scheme symbols, so it requires an ad hoc
  ;;hash function.  There are better hash functions than SIGNATURE-HASH,
  ;;but speed is also important.
  ;;
  (let* ((signature     (cons ret-type arg-types))
	 (callout-maker (hashtable-ref callout-maker-table signature #f)))
    (if callout-maker
	(callout-maker address)
      (let* ((callout-maker (ikarus:make-c-callout ret-type (%normalise-arg-types arg-types))))
	(hashtable-set! callout-maker-table signature callout-maker)
	(callout-maker address)))))

(define (make-c-callout/with-errno ret-type address arg-types)
  (let ((callout-closure (make-c-callout ret-type address arg-types)))
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

(define make-c-callback
  ;;Given a  signature of types, create a  callback closure constructor;
  ;;cache the closure constructors to avoid duplication.
  ;;
  ;;The signature is a list of  Scheme symbols, so it requires an ad hoc
  ;;hash function.  There are better hash functions than SIGNATURE-HASH,
  ;;but speed is also important.
  ;;
  (let ((callback-maker-table (make-hashtable %signature-hash equal?)))
    (lambda (ret-type scheme-function arg-types)
      (let* ((signature		(cons ret-type arg-types))
	     (callback-maker	(hashtable-ref callback-maker-table signature #f)))
	(if callback-maker
	    (callback-maker scheme-function)
	  (let ((callback-maker (ikarus:make-c-callback ret-type (%normalise-arg-types arg-types))))
	    (hashtable-set! callback-maker-table signature callback-maker)
	    (callback-maker scheme-function)))))))

(define (free-c-callback cb)
  #f)


;;;; done

)

;;; end of file

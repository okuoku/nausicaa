;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: foreign functions interface compatibility layer for Petite Chez
;;;Date: Sat Mar 20, 2010
;;;
;;;Abstract
;;;
;;;	Petite Chez up to version 7.9.4 does NOT support the full FFI of
;;;	Chez, so here we just define placeholders.
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (ffi platform)
  (export
    open-shared-object		lookup-shared-object
    make-c-callout		make-c-callout/with-errno
    make-c-callback		free-c-callback)
  (import (rnrs)
    (unimplemented)
    (only (ffi sizeof) LIBC_SHARED_OBJECT_SPEC)
    (ffi conditions)
    (ffi peekers-and-pokers)
    (prefix (only (chezscheme)
;;;	  foreign-procedure
		  load-shared-object)
	    petite:))

  ;; make-c-callout make-c-callback
  ;; errno pointer-set-c-int!)


;;;; helpers

(define-syntax foreign-procedure
  (syntax-rules ()
    ((_ ?func-name (?arg-type ...) ?ret-type)
     (raise-unimplemented-error 'foreign-procedure
				"Petite Chez Scheme does not support full FFI"
				?func-name))))


;;;; helpers

(define (%normalise-arg-types arg-types)
  (if (equal? '(void) arg-types)
      '()
    arg-types))

(define (%normalise-foreign-symbol foreign-symbol)
  (if (symbol? foreign-symbol)
      (symbol->string foreign-symbol)
    foreign-symbol))


;;;; dynamic loading

(define (open-shared-object library-name)
  ;;In case of error  PETITE:LOAD-SHARED-OBJECT raises an exception; the
  ;;second element of the &irritants list is a good error message.
  (guard (E (else
	     (raise-shared-object-opening-error 'open-shared-object
						(cadr (condition-irritants E))
						library-name)))
    (petite:load-shared-object (%normalise-foreign-symbol library-name))))

(define (lookup-shared-object library-pointer foreign-symbol)
  (raise-unimplemented-error 'lookup-shared-object
			     "Petite Chez Scheme does not support full FFI"))

  ;; (or (ikarus:dlsym library-pointer (%normalise-foreign-symbol foreign-symbol))
  ;;     (raise
  ;;      (condition (make-shared-object-lookup-error-condition)
  ;; 		  (make-who-condition 'lookup-shared-object)
  ;; 		  (make-message-condition (ikarus:dlerror))
  ;; 		  (make-irritants-condition (list library-pointer))
  ;; 		  (make-foreign-symbol-condition foreign-symbol)))))


;;;; errno interface

(define libc-shared-object
  (open-shared-object LIBC_SHARED_OBJECT_SPEC))

(define errno-location
  (foreign-procedure "__errno_location" (void) void*))

(define-syntax errno
  (syntax-rules ()
    ((_ ?value)
     (pointer-c-set! signed-int (errno-location) 0 ?value))
    ((_)
     (pointer-c-ref  signed-int (errno-location)))))


;;;; callout functions

(define (make-c-callout ret-type address arg-types)
  (raise-unimplemented-error 'lookup-shared-object
			     "Petite Chez Scheme does not support full FFI"))

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

(define (make-c-callback ret-type scheme-function arg-types)
  (raise-unimplemented-error 'make-c-callback
			     "Petite Chez Scheme does not support full FFI"))

  ;; (let ((code-object (foreign-callable scheme-function arg-types ret-type)))
  ;;   (lock-object code-object)
  ;;   code-object))

(define (free-c-callback cb)
  #f)


;;;; done

)

;;; end of file

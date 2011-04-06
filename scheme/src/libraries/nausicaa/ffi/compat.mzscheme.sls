;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility layer for Racket FFI
;;;Date: Wed Apr  6, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(library (nausicaa ffi compat)
  (export
    open-shared-object		lookup-shared-object
    make-c-callout		make-c-callout/with-errno
    make-c-callback		(rename (mosh:free-c-callback free-c-callback)))
  (import (rnrs)
    (nausicaa language unimplemented)
    (nausicaa ffi conditions)
    (only (nausicaa ffi sizeof) LIBC_SHARED_OBJECT_SPEC)
    (prefix (only (ffi unsafe)
		  ffi-lib		get-ffi-obj

		  ;; open-shared-library	lookup-shared-library
		  ;; make-c-function	pointer->c-function
		  ;; make-c-callback	free-c-callback
		  ;; shared-errno
		  )
	    racket.))


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
  ;;In  case of  error RACKET.FFI-LIB  raises an  exception.  Internally
  ;;Racket calls  "dlerror()" consuming the  error message, which  is no
  ;;more accessible  with further calls to "dlerror()".   The message is
  ;;accessible only in the &MESSAGE condition raised here.
  ;;
  (guard (E (else
	     (raise-shared-object-opening-error 'open-shared-object
						(condition-message E)
						library-name)))
    (racket.ffi-lib (%normalise-foreign-symbol library-name))))

(define libc-pointer
  (open-shared-object LIBC_SHARED_OBJECT_SPEC))

(define dlerror
  (racket.get-ffi-obj 'dlerror libc-pointer (_fun () -> char*)))

(define (lookup-shared-object library-pointer foreign-symbol)
  ;;RACKET.GET-FFI-OBJ raises returns  #f when the  symbol is not found  and it
  ;;does NOT call "dlerror()" (Mosh revision 2190), so the error message
  ;;would be avaiable here.
  ;;
  (let ((foreign-symbol (cond ((string? foreign-symbol)
			       (string->symbol foreign-symbol))
			      ((symbol? foreign-symbol)
			       foreign-symbol)
			      (else
			       (assertion-violation 'lookup-shared-object
				 "expected string or symbol as foreign symbol"
				 foreign-symbol)))))
    (or (racket.lookup-shared-library library-pointer foreign-symbol)
	(raise
	 (condition (make-shared-object-lookup-error-condition)
		    (make-who-condition 'lookup-shared-object)
		    (make-message-condition (dlerror))
		    (make-irritants-condition (list library-pointer))
		    (make-foreign-symbol-condition (symbol->string foreign-symbol)))))))


;;;; interface functions

(define (make-c-callout ret-type address arg-types)
  (racket.pointer->c-function address ret-type 'unknown (%normalise-arg-types arg-types)))

(define (make-c-callout/with-errno ret-type address arg-types)
  (let ((closure (make-c-callout ret-type address arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (racket.shared-errno 0)
			  (apply closure args)))
	     (errval	(racket.shared-errno)))
	(values retval errval)))))

(define (make-c-callback ret-type scheme-proc arg-types)
  (racket.make-c-callback ret-type (%normalise-arg-types arg-types) scheme-proc))


;;;; done

)

;;; end of file

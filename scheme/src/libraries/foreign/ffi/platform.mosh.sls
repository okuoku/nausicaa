;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: compatibility layer for Mosh FFI
;;;Date: Thu Jun 25, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    make-c-callback		(rename (mosh:free-c-callback free-c-callback)))
  (import (rnrs)
    (unimplemented)
    (foreign ffi conditions)
    (prefix (only (mosh ffi)
		  open-shared-library	lookup-shared-library
		  make-c-function	pointer->c-function
		  make-c-callback	free-c-callback
		  shared-errno
		  pointer-ref-c-uint8)
	    mosh:))


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
  ;;In  case  of  error  MOSH:OPEN-SHARED-LIBARRY raises  an  exception.
  ;;Internally  Mosh  (revision 2190)  calls  "dlerror()" consuming  the
  ;;error message,  which is  no more accessible  with further  calls to
  ;;"dlerror()".   The  message  is  accessible  only  in  the  &MESSAGE
  ;;condition raised here.
  (guard (E (else #f))
    (mosh:open-shared-library (%normalise-foreign-symbol library-name))))

(define (lookup-shared-object lib-spec foreign-symbol)
  ;;This   already  returns   #f   when  the   symbol   is  not   found.
  ;;MOSH:LOOKUP-SHARED-LIBRARY does NOT  call "dlerror()" (Mosh revision
  ;;2190), so the error message would be avaiable here.
  (mosh:lookup-shared-library lib-spec (if (string? foreign-symbol)
					   (string->symbol foreign-symbol)
					 foreign-symbol)))


;;;; interface functions

(define (make-c-callout ret-type address arg-types)
  (mosh:pointer->c-function address ret-type 'unknown (%normalise-arg-types arg-types)))

(define (make-c-callout/with-errno ret-type address arg-types)
  (let ((closure (make-c-callout ret-type address arg-types)))
    (lambda args
      ;;We have to use LET* here  to enforce the order of evaluation: we
      ;;want  to gather  the "errno"  value AFTER  the  foreign function
      ;;call.
      (let* ((retval	(begin
			  (mosh:shared-errno 0)
			  (apply closure args)))
	     (errval	(mosh:shared-errno)))
	(values retval errval)))))

(define (make-c-callback ret-type scheme-proc arg-types)
  (mosh:make-c-callback ret-type (%normalise-arg-types arg-types) scheme-proc))


;;;; done

)

;;; end of file

;;;
;;;Part of: Uriel libraries for Ikarus
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;Time-stamp: <2008-11-24 17:59:34 marco>
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

    ;;interface functions
    shared-object open-shared-object make-c-function define-c-function

    ;;memory functions
    malloc primitive-malloc primitive-free
    make-out-of-memory-condition out-of-memory-condition?
    out-of-memory-requested-number-of-bytes

    ;;string functions
    strlen string->cstring cstring->string)

  (import (rnrs)
    (uriel ffi compat))



;;;; dynamic loading

(define (open-shared-object library-name)
  (primitive-open-shared-object (if (symbol? library-name)
				    (symbol->string library-name)
				  library-name)))

(define-syntax make-c-function
  (syntax-rules ()
    ((_ ?ret-type ?funcname (?arg-type0 ?arg-type ...))
     (primitive-make-c-function
      ?ret-type ?funcname (?arg-type0 ?arg-type ...)))))

(define-syntax define-c-function
  (syntax-rules ()
    ((_ ?name (?ret-type ?funcname (?arg-type0 ?arg-type ...)))
     (define ?name
       (make-c-function
	?ret-type ?funcname (?arg-type0 ?arg-type ...))))))



;;;; memory functions

(define-condition-type &out-of-memory &error
  make-out-of-memory-condition out-of-memory-condition?
  (number-of-bytes out-of-memory-requested-number-of-bytes))

(define (malloc size)
  (let ((p (primitive-malloc size)))
    (unless p
      (make-out-of-memory-condition size))
    p))


;;;; done

)

;;; end of file

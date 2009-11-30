;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: condition objects for FFI
;;;Date: Sun Nov  1, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(library (foreign ffi conditions)
  (export
    &library-name
    make-library-name-condition
    library-name-condition?
    condition-library-name

    &shared-object
    make-shared-object-condition
    shared-object-condition?
    condition-shared-object

    &foreign-symbol
    make-foreign-symbol-condition
    foreign-symbol-condition?
    condition-foreign-symbol

    &unknown-shared-object-error
    make-unknown-shared-object-error-condition
    unknown-shared-object-error-condition?

    &unknown-foreign-symbol-error
    make-unknown-foreign-symbol-error-condition
    unknown-foreign-symbol-error-condition?

    raise-unknown-shared-object
    raise-unknown-foreign-symbol)
  (import (rnrs))


(define-condition-type &library-name &condition
  make-library-name-condition library-name-condition?
  (object condition-library-name))

(define-condition-type &shared-object &condition
  make-shared-object-condition shared-object-condition?
  (object condition-shared-object))

(define-condition-type &foreign-symbol &condition
  make-foreign-symbol-condition foreign-symbol-condition?
  (symbol condition-foreign-symbol))


(define-condition-type &unknown-shared-object-error &error
  make-unknown-shared-object-error-condition
  unknown-shared-object-error-condition?)

(define-condition-type &unknown-foreign-symbol-error &error
  make-unknown-foreign-symbol-error-condition
  unknown-foreign-symbol-error-condition?)

(define-syntax raise-unknown-shared-object
  (syntax-rules ()
    ((_ ?who ?message ?library-name)
     (raise-continuable
      (condition (make-unknown-shared-object-error-condition)
		 (make-who-condition ?who)
		 (make-message-condition ?message)
		 (make-library-name-condition ?library-name))))))

(define-syntax raise-unknown-foreign-symbol
  (syntax-rules ()
    ((_ ?who ?message ?shared-object ?foreign-symbol)
     (raise-continuable
      (condition (make-unknown-foreign-symbol-error-condition)
		 (make-who-condition ?who)
		 (make-message-condition ?message)
		 (make-shared-object-condition ?shared-object)
		 (make-foreign-symbol-condition ?foreign-symbol))))))


;;;; done

)

;;; end of file

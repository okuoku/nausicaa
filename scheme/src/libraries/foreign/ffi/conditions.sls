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
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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

    &shared-object-opening-error
    make-shared-object-opening-error-condition
    shared-object-opening-error-condition?

    &shared-object-lookup-error
    make-shared-object-lookup-error-condition
    shared-object-lookup-error-condition?

    raise-shared-object-opening-error
    raise-shared-object-lookup-error)
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


(define-condition-type &shared-object-opening-error &error
  make-shared-object-opening-error-condition
  shared-object-opening-error-condition?)

(define-condition-type &shared-object-lookup-error &error
  make-shared-object-lookup-error-condition
  shared-object-lookup-error-condition?)

(define-syntax raise-shared-object-opening-error
  (syntax-rules ()
    ((_ ?who ?message ?library-name)
     (raise-continuable
      (condition (make-shared-object-opening-error-condition)
		 (make-who-condition ?who)
		 (make-message-condition ?message)
		 (make-library-name-condition ?library-name))))))

(define-syntax raise-shared-object-lookup-error
  (syntax-rules ()
    ((_ ?who ?message ?shared-object ?foreign-symbol)
     (raise-continuable
      (condition (make-shared-object-lookup-error-condition)
		 (make-who-condition ?who)
		 (make-message-condition ?message)
		 (make-shared-object-condition ?shared-object)
		 (make-foreign-symbol-condition ?foreign-symbol))))))


;;;; done

)

;;; end of file

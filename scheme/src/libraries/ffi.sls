;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: foreign function interface extensions
;;;Date: Tue Nov 18, 2008
;;;
;;;Abstract
;;;
;;;	This is the core of the foreign functions interface.
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


(library (ffi)
  (export

;;; bindings from (ffi conditions)
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
    raise-shared-object-lookup-error

;;; bindings from (ffi primitives)
    shared-object?			libc-shared-object
    open-shared-object			lookup-shared-object
    make-c-function			make-c-function/with-errno
    make-c-callout			make-c-callout/with-errno
    make-c-callback			free-c-callback
    define-c-struct-accessor		define-c-struct-mutator
    define-c-struct-accessor-and-mutator
    define-c-struct-field-pointer-accessor
    define-c-struct-accessor-and-mutator/from-type

;;; bindings from (ffi utilities)
    define-shared-object
    define-c-functions			define-c-functions/with-errno
    define-c-callouts			define-c-callouts/with-errno
    make-c-function*			make-c-function/with-errno*
    make-c-callout*			make-c-callout/with-errno*
    make-c-callback*

;;; bindings from (ffi pointers)
    pointer?
    pointer-null			pointer-null?
    integer->pointer			pointer->integer
    pointer-diff			pointer-add
    pointer-incr!
    pointer=?				pointer<>?
    pointer<?				pointer>?
    pointer<=?				pointer>=?

;;; bindings from (ffi peekers-and-pokers)
    pointer-c-ref			pointer-c-set!
    pointer-c-accessor			pointer-c-mutator
    array-c-ref				array-c-set!
    array-c-pointer-to)
  (import (rnrs)
    (ffi conditions)
    (ffi pointers)
    (ffi peekers-and-pokers)
    (ffi primitives)
    (ffi utilities)))

;;; end of file

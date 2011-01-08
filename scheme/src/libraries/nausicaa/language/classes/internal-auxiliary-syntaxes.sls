;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: internal auxiliary syntaxes for classes
;;;Date: Mon Nov  1, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language classes internal-auxiliary-syntaxes)
  (export
    :class-record-type-descriptor
    :class-type-uid
    :class-uid-list
    :from-fields-constructor-descriptor
    :is-a?
    :predicate
    :list-of-concrete-fields
    :list-of-methods
    :list-of-virtual-fields
    :make
    :make*
    :make-from-fields
    :parent-rtd-list
    :public-constructor-descriptor
    :superclass-constructor-descriptor
    :superclass-protocol
    :with-class-bindings-of
    :slot-accessor
    :slot-mutator
    :list-of-superclasses
    :list-of-field-tags)
  (import (rnrs)
    (only (nausicaa language syntax-utilities) define-auxiliary-syntaxes))
  (define-auxiliary-syntaxes
    :class-record-type-descriptor
    :class-type-uid
    :class-uid-list
    :from-fields-constructor-descriptor
    :is-a?
    :predicate
    :list-of-concrete-fields
    :list-of-methods
    :list-of-virtual-fields
    :make
    :make*
    :make-from-fields
    :parent-rtd-list
    :public-constructor-descriptor
    :superclass-constructor-descriptor
    :superclass-protocol
    :with-class-bindings-of
    :slot-accessor
    :slot-mutator
    :list-of-superclasses
    :list-of-field-tags))

;;; end of file

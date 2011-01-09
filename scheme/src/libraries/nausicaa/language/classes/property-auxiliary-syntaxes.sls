;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: definition of auxiliary syntaxes for identifier properties
;;;Date: Sun Jan  9, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language classes property-auxiliary-syntaxes)
  (export
    :list-of-superclasses
    :list-of-field-tags
    :field-specs
    :virtual-field-specs
    :method-specs

    ;; used by DEFINE-MIXIN and the MIXINS clause
    :mixin-clauses)
  (import (only (nausicaa language syntax-utilities) define-auxiliary-syntaxes))
  (define-auxiliary-syntaxes
    :list-of-superclasses
    :list-of-field-tags
    :field-specs
    :virtual-field-specs
    :method-specs

    :mixin-clauses))

;;; end of file

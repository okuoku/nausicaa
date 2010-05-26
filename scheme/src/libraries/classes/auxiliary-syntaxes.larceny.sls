;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: auxiliary syntaxes for class and label definitions
;;;Date: Wed May 26, 2010
;;;
;;;Abstract
;;;
;;;	Export one binding for  each clause accepted by DEFINE-CLASS and
;;;	DEFINE-LABEL in the (classes) library.
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
(library (classes auxiliary-syntaxes)
  (export
    ;; bindings  which should be  from (rnrs records syntactic  (6)) but
    ;; are not under Larceny
    parent sealed opaque parent-rtd nongenerative
    protocol fields mutable immutable

    ;; custom bindings
    inherit predicate setter getter bindings
    public-protocol superclass-protocol virtual-fields
    methods method method-syntax)
  (import (rnrs))
  (define-syntax define-auxiliary-syntax
    (syntax-rules ()
      ((_ ?name)
       (define-syntax ?name (identifier-syntax #f)))))

  (define-auxiliary-syntax parent)
  (define-auxiliary-syntax sealed)
  (define-auxiliary-syntax opaque)
  (define-auxiliary-syntax parent-rtd)
  (define-auxiliary-syntax nongenerative)
  (define-auxiliary-syntax protocol)
  (define-auxiliary-syntax fields)
  (define-auxiliary-syntax mutable)
  (define-auxiliary-syntax immutable)

  (define-auxiliary-syntax inherit)
  (define-auxiliary-syntax predicate)
  (define-auxiliary-syntax setter)
  (define-auxiliary-syntax getter)
  (define-auxiliary-syntax bindings)
  (define-auxiliary-syntax public-protocol)
  (define-auxiliary-syntax superclass-protocol)
  (define-auxiliary-syntax virtual-fields)
  (define-auxiliary-syntax methods)
  (define-auxiliary-syntax method)
  (define-auxiliary-syntax method-syntax))

;;; end of file

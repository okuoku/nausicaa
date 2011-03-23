;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: auxiliary syntaxes
;;;Date: Tue May 25, 2010
;;;
;;;Abstract
;;;
;;;	Export one binding for  each clause accepted by DEFINE-CLASS and
;;;	DEFINE-LABEL  in  the  (classes)  library.  Exports  also  other
;;;	auxiliary  syntaxes, like  the ones  for the  vector  and string
;;;	views.
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(library (nausicaa language auxiliary-syntaxes)
  (export
    ;; bindings from (rnrs records syntactic (6))
    parent sealed opaque parent-rtd nongenerative
    protocol fields mutable immutable

    ;; custom bindings for (classes)
    inherit predicate maker maker-transformer setter getter bindings
    public-protocol maker-protocol superclass-protocol virtual-fields
    methods method method-syntax custom-maker mixins satisfies

    ;; makers
    mandatory optional with without

    ;; bindings for string and vector views
    view start past

    ;; for MATCH
    :predicate
    :accessor
    :and
    :or
    :not
    :setter
    :getter
    :free-identifier
    :bound-identifier

    ;; for generic functions
    uid-list-of reverse-before-methods merge
    :primary :before :after :around

    ;; miscellaneous
    <> <...>)
  (import (rnrs records syntactic)
    (only (rnrs base) begin define-syntax syntax-rules ...))


;;This  library is  imported by  (nausicaa language  extensions),  so we
;;cannot import from there the binding for DEFINE-AUXILIARY-SYNTAXES.
(define-syntax def
  (syntax-rules ()
    ((_ ?id ...)
     (begin
       (define-syntax ?id (syntax-rules ()))
       ...))))

(def inherit
     predicate
     maker
     maker-transformer
     custom-maker
     setter
     getter
     bindings
     public-protocol
     maker-protocol
     superclass-protocol
     virtual-fields
     methods
     method
     method-syntax
     mixins
     satisfies

     <>
     <...>

     ;; makers
     mandatory optional with without

     ;; string and vector views
     view
     start
     past

     ;; for MATCH
     :predicate
     :accessor
     :and
     :or
     :not
     :setter
     :getter
     :free-identifier
     :bound-identifier

     ;; for generic functions
     uid-list-of
     reverse-before-methods
     merge
     :primary
     :before
     :after
     :around
     )


;;;; done

)

;;; end of file

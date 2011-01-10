;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: definition class properties infrastructure
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
(library (nausicaa language classes properties)
  (export
    struct-properties-ref	struct-properties-define
    mixin-clauses-ref		mixin-clauses-define

    struct?
    struct-list-of-supers
    struct-field-specs
    struct-virtual-field-specs
    struct-method-specs
    struct-mixins
    struct-list-of-field-tags

    make-class
    class?
    (rename (struct-list-of-supers		class-list-of-supers)
	    (struct-field-specs			class-field-specs)
	    (struct-virtual-field-specs		class-virtual-field-specs)
	    (struct-method-specs		class-method-specs)
	    (struct-mixins			class-mixins)
	    (struct-list-of-field-tags		class-list-of-field-tags))

    make-mixin
    mixin?
    (rename (struct-list-of-supers		mixin-list-of-supers)
	    (struct-field-specs			mixin-field-specs)
	    (struct-virtual-field-specs		mixin-virtual-field-specs)
	    (struct-method-specs		mixin-method-specs)
	    (struct-mixins			mixin-mixins)
	    (struct-list-of-field-tags		mixin-list-of-field-tags))

    make-label
    label?
    (rename (struct-list-of-supers		label-list-of-supers)
	    (struct-field-specs			label-field-specs)
	    (struct-virtual-field-specs		label-virtual-field-specs)
	    (struct-method-specs		label-method-specs)
	    (struct-mixins			label-mixins)
	    (struct-list-of-field-tags		label-list-of-field-tags)))
  (import (rnrs)
    (prefix (nausicaa language identifier-properties) ip.)
    (for (nausicaa language classes top) (meta -1))
    (only (nausicaa language extensions)
	  define-auxiliary-syntaxes))


(define-auxiliary-syntaxes
  :struct-properties
  :mixin-clauses)

(define (struct-properties-ref identifier)
  (ip.ref identifier #':struct-properties #f))

(define (struct-properties-define identifier value)
  (ip.define identifier #':struct-properties value))

(define (mixin-clauses-ref identifier)
  (ip.ref identifier #':mixin-clauses #f))

(define (mixin-clauses-define identifier value)
  (ip.define identifier #':mixin-clauses value))


(define-record-type struct
  (nongenerative nausicaa:language:classes:properties:struct)
  (fields (immutable list-of-supers)
	  (immutable field-specs)
	  (immutable virtual-field-specs)
	  (immutable method-specs)
	  (immutable mixins)
	  (immutable list-of-field-tags)))

(define-record-type class
  (nongenerative nausicaa:language:classes:properties:class)
  (parent struct)
  (opaque #f)
  (sealed #f))

(define-record-type mixin
  (nongenerative nausicaa:language:classes:properties:mixin)
  (parent struct)
  (opaque #f)
  (sealed #f))

(define-record-type label
  (nongenerative nausicaa:language:classes:properties:label)
  (parent struct)
  (opaque #f)
  (sealed #f)
  (protocol (lambda (make-st)
	      (lambda ( ;;
		  list-of-supers virtual-field-specs
		  method-specs mixins list-of-field-tags)
		((make-st list-of-supers '() virtual-field-specs
			  method-specs mixins list-of-field-tags))))))


;;;; done

;;This should be in  the top library; it is here to  circumvent a bug in
;;Ikarus/Vicare precompiled  libraries which is causing  the property to
;;be misteriously not  set when it is queried  from the helpers library;
;;the bug shows itself only when running with precompiled libraries, not
;;when running with a clean cache (Marco Maggi; Mon Jan 10, 2011).
;;
(struct-properties-define
 #'<top> (make-class '()   ;list of supers
		     '()   ;field specs
		     '()   ;virtual field specs
		     '()   ;method specs
		     '()   ;mixins
		     '())) ;list of field tags

)

;;; end of file

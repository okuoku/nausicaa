;;;
;;;Part of: Uriel libraries
;;;Contents: helpers for CLOS
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;	This library of helper functions is meant to be portable to both
;;;	Tiny-CLOS and Nausicaa/ScmObj.
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


;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (scmobj utils)
  (export
    prepend-to-slot append-to-slot
    with-slots-set! with-slots-ref
    with-slots)
  (import (r6rs)
    (scmobj))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Special slot accessors.
;;; --------------------------------------------------------------------

(define-syntax prepend-to-slot
  (syntax-rules (quote)
    ((_ ?object ?slot ?value)
     (let ((slot-name ?slot)
	   (object ?object))
       (slot-set! object slot-name
		  (cons ?value (slot-ref object slot-name)))))))

(define-syntax append-to-slot
  (syntax-rules (quote)
    ((_ ?object ?slot ?value)
     (let ((slot-name ?slot)
	   (object ?object))
       (slot-set! object slot-name
		  (append (slot-ref object slot-name) (list ?value)))))))

;;; --------------------------------------------------------------------

(define-syntax with-slots-set!
  (syntax-rules (quote)
    ((_ ?object (quote (?slot0 ?slot ...)) (quote (?value0 ?value ...)))
     (with-slots-set! ?object (?slot0 ?slot ...) (?value0 ?value ...)))
    ((_ ?object (?slot0 ?slot ...) (?value0 ?value ...))
     (for-each
	 (lambda (s v) (slot-set! ?object s v))
       (quote (?slot0 ?slot ...)) (quote (?value0 ?value ...))))))

(define-syntax with-slots-ref
  (syntax-rules (quote)
    ((_ ?object (quote (?slot0 ?slot ...)))
     (with-slots-ref ?object (?slot0 ?slot ...)))
    ((_ ?object (?slot0 ?slot ...))
     (map
	 (lambda (s) (slot-ref ?object s))
       (quote (?slot0 ?slot ...))))))

;;; --------------------------------------------------------------------

;;Example:
;;
;;  (define-class <C> () (a b c))
;;  (define A (make <C>))
;;  (define B (make <C>))
;;
;;  (with-slots (((d e f) (a b c) A)
;;               ((g h i) (a b c) B))
;;     (do-something d e f g h i))
;;
(define-syntax with-slots
  (syntax-rules (quote)
    ((_ () ?form0 ?form ...)
     (begin ?form0 ?form ...))
    ((_ (((?sym0 ?sym ...) (?slot0 ?slot ...) ?object)
	 ((?sym10 ?sym1 ...) (?slot10 ?slot1 ...) ?object1)
	 ...)
	?form0 ?form ...)
     (let ((?sym0 (slot-ref ?object (quote ?slot0)))
	   (?sym (slot-ref ?object (quote ?slot)))
	   ...)
       (with-slots (((?sym10 ?sym1 ...) (?slot10 ?slot1 ...) ?object1)
		    ...)
	 ?form0 ?form ...)))))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

) ;; end of library form

;;; end of file

;;;
;;;Part of: Uriel libraries
;;;Contents: helpers for CLOS
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(library (uriel clos)
  (export a)
  (import (rnrs)
    (clos core))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Code.
;;; --------------------------------------------------------------------

(define-syntax prepend-to-slot
  (syntax-rules ()
    ((_ ?object ?slot ?value)
     (slot-set! ?object (quote ?slot)
		(cons ?value (slot-ref ?object (quote ?slot)))))))

(define-syntax append-to-slot
  (syntax-rules ()
    ((_ ?object ?slot ?value)
     (slot-set! ?object (quote ,slot)
		(append (slot-ref ?object (quote ?slot)) (list ?value))))))

;; ------------------------------------------------------------

(define-syntax with-slots-set!
  (syntax-rules ()
    ((_ ?object (?slot0 ?slot ...) (?value0 ?value ...))
     (for-each
	 (lambda (s v) (slot-set! ?object s v))
       (?slot0 ?slot ...) (?value0 ?value ...)))))

(define-syntax with-slots-ref
  (syntax-rules ()
    ((_ ?object (?slot0 ?slot ...))
     (map
	 (lambda (s) (slot-ref ?object s))
       (?slot0 ?slot ...)))))

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
  (syntax-rules ()
    ((_ (((?sym ...) (?slot ...) ?object)
	 ...) ?form0 ?form ...)
     (let ((?sym (slot-ref ?object (quote ?slot)))
	   ...)
       ?form0 ?form ...))))

    ;;This is  here only to let  me remove the  bindings while debugging
    ;;the form.
;;     ((_ ((() () ?object)) ?form ...)
;;      (begin ?form ...))

;;     ((_ (((?sym ...) (?slot ...) ?object)) ?form ...)
;;      (let ((?sym (slot-ref ?object '?slot))
;; 	   ...)
;;        ?form ...))

;;     ((_ (((?sym1 ...) (?slot1 ...) ?object1)
;; 	 ((?sym2 ...) (?slot2 ...) ?object2)
;; 	 ...) ?form0 ?form ...)
;;      (with-slots (((?sym1 ...) (?slot1 ...) ?object1))
;;        (with-slots (((?sym2 ...) (?slot2 ...) ?object2) ...)
;; 	 ?form ?form ...)))))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

) ;; end of library form

;;; end of file

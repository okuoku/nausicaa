;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: Nausicaa Object System slots stuff
;;;Date: Wed Aug 26, 2009
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


(library (nos slots)
  (export

    make-slot			clone-slot			slot?

;;     make-class-slot		class-slot?
;;     make-slots-collection	slots-collection-ref

    ;; record getters
    slot-init-thunk		slot-accessor			slot-mutator

;;     class-slot-name		class-slot-predicate		class-slot-assertion
;;     class-slot-init-value	class-slot-init-keyword		class-slot-init-form
;;     class-slot-init-thunk
;;     class-slot-getter		class-slot-mutator		class-slot-accessor
    )
  (import (rnrs)
    (keywords)
    (nos keywords)
    (sentinel))


;;;; helpers

(define %lambda-sentinel
  (lambda () sentinel))



(define-record-type (slot slot-make slot?)
  (fields (immutable init-thunk)
	  (immutable accessor)
	  (immutable mutator)))

(define-syntax make-slot
  (syntax-rules ()
    ((k ?arg ...)
     (let-keywords (list ?arg ...) #f
		   ((init-value		:init-value		sentinel)
		    (init-thunk		:init-thunk		#f)
		    (accessor		:accessor		#f)
		    (accessor-before	:accessor-before	#f)
		    (accessor-after	:accessor-after		#f)
		    (mutator		:mutator		#f)
		    (mutator-before	:mutator-before		#f)
		    (mutator-after	:mutator-after		#f))

       (slot-make (or init-thunk (lambda () init-value))

		  (if accessor
		      (cond ((and accessor-before accessor-after)
			     (lambda (object)
			       (dynamic-wind
				   (lambda () (accessor-before object))
				   (lambda () (accessor object))
				   (lambda () (accessor-after object)))))
			    (accessor-before
			     (lambda (object)
			       (dynamic-wind
				   (lambda () (accessor-before object))
				   (lambda () (accessor object))
				   %lambda-sentinel)))
			    (accessor-after
			     (lambda (object)
			       (dynamic-wind
				   %lambda-sentinel
				   (lambda () (accessor object))
				   (lambda () (accessor-after object)))))
			    (else
			     accessor))
		    #f)

		  (if mutator
		      (cond ((and mutator-before mutator-after)
			     (lambda (object value)
			       (dynamic-wind
				   (lambda () (mutator-before object value))
				   (lambda () (mutator        object value))
				   (lambda () (mutator-after  object value)))))
			    (mutator-before
			     (lambda (object value)
			       (dynamic-wind
				   (lambda () (mutator-before object value))
				   (lambda () (mutator        object value))
				   %lambda-sentinel)))
			    (mutator-after
			     (lambda (object value)
			       (dynamic-wind
				   %lambda-sentinel
				   (lambda () (mutator       object value))
				   (lambda () (mutator-after object value)))))
			    (else
			     mutator))
		    #f))))))

(define-syntax clone-slot
  (syntax-rules ()
    ((_ ?slot)
     (let ((S ?slot))
       (slot-make (slot-init-thunk	S)
		  (slot-init-accessor	S)
		  (slot-init-mutator	S))))))


(define-record-type (class-slot class-slot-make class-slot?)
  (fields (immutable name)
	  (immutable slot		:class-slot-slot)
 	  (immutable accessor)
 	  (immutable mutator)
 	  (immutable accessor/mutator)))

(define-syntax make-class-slot
  (syntax-rules ()
    ((_ ?name ?slot ?accessor ?mutator)
     (let ((accessor	?accessor)
	   (mutator	?mutator))
       (:class-slot-make ?name ?slot accessor mutator
			 (case-lambda
			  ((object)		(accessor object))
			  ((object value)	(mutator  object value))))))))


;; (define (%make-class-slots-collection . class-slots)
;;   (map (lambda (class-slot)
;; 	 (cons (class-slot-slot slot) class-slot))
;;     class-slots))

;; (define-syntax %class-slots-collection-ref
;;   (syntax-rules ()
;;     ((_ ?class-slots-collection ?slot)
;;      (let loop ((class-slots ?class-slots-collection))
;;        (cond ((null? class-slots)
;; 	      #f)
;; 	     ((eq? ?slot (class-slot-slot (car class-slots)))
;; 	      (car class-slots))
;; 	     (else (loop (cdr class-slots))))))))

;; (define-syntax slot-getter
;;   (syntax-rules ()
;;     ((_ ?class ?slot)
;;      (class-slot-getter
;;       (%class-slots-collection-ref (class-slots ?class) ?slot)))))

;; (define-syntax slot-mutator
;;   (syntax-rules ()
;;     ((_ ?class ?slot)
;;      (class-slot-mutator
;;       (%class-slots-collection-ref (class-slots ?class) ?slot)))))

;; (define-syntax slot-ref
;;   (syntax-rules ()
;;     ((_ ?instance ?slot)
;;      (let ((instance ?instance))
;;        ((slot-getter (instance-class instance) ?slot) instance)))))

;; (define-syntax slot-set!
;;   (syntax-rules ()
;;     ((_ ?instance ?slot ?value)
;;      (let ((instance ?instance))
;;        ((slot-getter (instance-class instance) ?slot) instance ?value)))))


;;;; done

)

;;; end of file

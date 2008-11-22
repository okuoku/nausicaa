;;;
;;;Part of: Uriel libraries for Ikarus Scheme
;;;Contents: object property library
;;;Date: Fri Nov 14, 2008
;;;Time-stamp: <2008-11-22 07:43:55 marco>
;;;
;;;Abstract
;;;
;;;
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



;;;; setup

(library (uriel object-property)
  (export
    object-property-initial-capacity
    object-property-default-value
    make-object-property with-true-property)
  (import (rnrs)
    (srfi parameters))


;;;; code

(define object-property-initial-capacity
  (make-parameter 100
    (lambda (initial-capacity)
      (assert (integer? initial-capacity))
      initial-capacity)))

(define object-property-default-value
  (make-parameter #f))

(define (make-object-property)
  (let ((table		(make-eq-hashtable
			 (object-property-initial-capacity)))
	(default-value	(object-property-default-value)))
    (case-lambda
     ((object)
      (hashtable-ref table object default-value))
     ((object value)
      (hashtable-set! table object value)))))

(define-syntax with-true-property
  (syntax-rules ()
    ((_ (?prop ?object) ?form0 ?form ...)
     (let ((saved (?prop ?object)))
       (dynamic-wind
	   (lambda () (?prop ?object #t))
	   (lambda () ?form0 ?form ...)
	   (lambda () (?prop ?object saved)))))))



) ;; end of library form

;;; end of file

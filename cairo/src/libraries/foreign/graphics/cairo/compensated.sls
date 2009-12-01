;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Cairo
;;;Contents: compensated constructors
;;;Date: Tue Dec  1, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign graphics cairo compensated)
  (export

    cairo-create/c
    cairo-reference/c
    cairo-save/c
    cairo-push-group/c
    cairo-push-group-to-source/c
    cairo-push-group/content/c
    cairo-push-group-to-source/content/c

    cairo-font-options-create/c
    cairo-font-options-copy/c
    cairo-scaled-font-reference/c
    cairo-toy-font-face-create/c
    cairo-user-font-face-create/c

    cairo-surface-create-similar/c
    cairo-surface-reference/c
    cairo-image-surface-create/c
    cairo-image-surface-create-from-png/c
    cairo-image-surface-create-from-png-stream/c

    cairo-pattern-create-rgb/c
    cairo-pattern-create-rgba/c
    cairo-pattern-create-for-surface/c
    cairo-pattern-create-linear/c
    cairo-pattern-create-radial/c
    cairo-pattern-reference/c

    cairo-pdf-surface-create/c
    cairo-pdf-surface-create-for-stream/c

    cairo-svg-surface-create/c
    cairo-svg-surface-create-for-stream/c

    cairo-ps-surface-create/c
    cairo-ps-surface-create-for-stream/c

    cairo-xlib-surface-create-with-xrender-format/c

    cairo-ft-font-face-create-for-pattern/c
    cairo-ft-font-face-create-for-ft-face/c

    cairo-xlib-surface-create/c
    cairo-xlib-surface-create-for-bitmap/c)
  (import (rnrs)
    (compensations)
    (foreign graphics cairo))


;;;; compensations

(define-syntax define-compensated
  (syntax-rules ()
    ((_ ?name ?constructor ?destructor)
     (define (?name . args)
       (letrec ((obj (compensate
			 (apply ?constructor args)
		       (with
			(?destructor obj)))))
	 obj)))))

;;; --------------------------------------------------------------------

(define-compensated cairo-create/c
  cairo-create cairo-destroy)

(define-compensated cairo-reference/c
  cairo-reference cairo-destroy)

(define (cairo-save/c cr)
  (compensate
      (cairo-save cr)
    (with
     (cairo-restore cr))))

(define (cairo-push-group/c cr)
  (compensate
      (cairo-push-group cr)
    (with
     (cairo-pop-group cr))))

(define (cairo-push-group-to-source/c cr)
  (compensate
      (cairo-push-group cr)
    (with
     (cairo-pop-group-to-source cr))))

(define (cairo-push-group/content/c cr content)
  (compensate
      (cairo-push-group-with-content cr content)
    (with
     (cairo-pop-group cr))))

(define (cairo-push-group-to-source/content/c cr content)
  (compensate
      (cairo-push-group-with-content cr content)
    (with
     (cairo-pop-group-to-source cr))))

;;; --------------------------------------------------------------------

(define-compensated cairo-font-options-create/c
  cairo-font-options-create cairo-font-options-destroy)

(define-compensated cairo-font-options-copy/c
  cairo-font-options-copy cairo-font-options-destroy)

(define-compensated cairo-scaled-font-reference/c
  cairo-scaled-font-reference cairo-scaled-font-destroy)

(define-compensated cairo-toy-font-face-create/c
  cairo-toy-font-face-create cairo-scaled-font-destroy)

(define-compensated cairo-user-font-face-create/c
  cairo-user-font-face-create cairo-scaled-font-destroy)

;;; --------------------------------------------------------------------

(define-compensated cairo-surface-create-similar/c
  cairo-surface-create-similar cairo-surface-destroy)

(define-compensated cairo-surface-reference/c
  cairo-surface-reference cairo-surface-destroy)

(define-compensated cairo-image-surface-create/c
  cairo-image-surface-create cairo-surface-destroy)

(define-compensated cairo-image-surface-create-from-png/c
  cairo-image-surface-create-from-png cairo-surface-destroy)

(define-compensated cairo-image-surface-create-from-png-stream/c
  cairo-image-surface-create-from-png-stream cairo-surface-destroy)

;;; --------------------------------------------------------------------

(define-compensated cairo-pattern-create-rgb/c
  cairo-pattern-create-rgb cairo-pattern-destroy)

(define-compensated cairo-pattern-create-rgba/c
  cairo-pattern-create-rgba cairo-pattern-destroy)

(define-compensated cairo-pattern-create-for-surface/c
  cairo-pattern-create-for-surface cairo-pattern-destroy)

(define-compensated cairo-pattern-create-linear/c
  cairo-pattern-create-linear cairo-pattern-destroy)

(define-compensated cairo-pattern-create-radial/c
  cairo-pattern-create-radial cairo-pattern-destroy)

(define-compensated cairo-pattern-reference/c
  cairo-pattern-reference cairo-pattern-destroy)

;;; --------------------------------------------------------------------

(define-compensated cairo-pdf-surface-create/c
   cairo-pdf-surface-create cairo-surface-destroy)

(define-compensated cairo-pdf-surface-create-for-stream/c
  cairo-pdf-surface-create-for-stream cairo-surface-destroy)

;;; --------------------------------------------------------------------

(define-compensated cairo-svg-surface-create/c
  cairo-svg-surface-create cairo-surface-destroy)

(define-compensated cairo-svg-surface-create-for-stream/c
  cairo-svg-surface-create-for-stream cairo-surface-destroy)

;;; --------------------------------------------------------------------

(define-compensated cairo-ps-surface-create/c
  cairo-ps-surface-create cairo-surface-destroy)

(define-compensated cairo-ps-surface-create-for-stream/c
  cairo-ps-surface-create-for-stream cairo-surface-destroy)

;;; --------------------------------------------------------------------

(define-compensated cairo-xlib-surface-create-with-xrender-format/c
  cairo-xlib-surface-create-with-xrender-format cairo-surface-destroy)

;;; --------------------------------------------------------------------

(define-compensated cairo-ft-font-face-create-for-pattern/c
  cairo-ft-font-face-create-for-pattern cairo-scaled-font-destroy)

(define-compensated cairo-ft-font-face-create-for-ft-face/c
  cairo-ft-font-face-create-for-ft-face cairo-scaled-font-destroy)

;;; --------------------------------------------------------------------

(define-compensated cairo-xlib-surface-create/c
  cairo-xlib-surface-create cairo-surface-destroy)

(define-compensated cairo-xlib-surface-create-for-bitmap/c
  cairo-xlib-surface-create-for-bitmap cairo-surface-destroy)


;;;; done

)

;;; end of file

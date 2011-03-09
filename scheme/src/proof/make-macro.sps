;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: just a proof for a make macro object systems
;;;Date: Sun Jul  5, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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

(import (rnrs))

(define (%make-instance class-name class . slots)
  (write (list class-name class slots))
  (newline))

;; (define-syntax slot
;;   (lambda (stx)
;;     (define (class-slots class)
;;       class)
;;     (define (doit class key)
;;       (unless (memq key (class-slots class))
;; 	(syntax-violation 'slot "invalid slot keyword" key)))
;;     (syntax-case stx ()
;;       ((_ ?class ?key ?value)
;;        (with-syntax ((?exp (doit (syntax->datum (syntax ?class))
;; 				 (syntax->datum (syntax ?key)))))
;; 	 (syntax '(?key . ?value)))))))

(define (class-slots class)
  (cdar class))

(define (%slot class key value)
  (if (memq key (class-slots class))
      (cons key value)
    (syntax-violation 'slot "invalid slot keyword" key)))

(define-syntax make-instance
  (syntax-rules (:slots)
    ((_ ?class (:slots (?key0 . ?value0) ...) ?key ?value ?thing ...)
     (make-instance ?class (:slots (?key . ?value) (?key0 . ?value0) ...) ?thing ...))
    ((_ ?class (:slots (?key . ?value) ...))
     (%make-instance (quote ?class) ?class (%slot ?class (quote ?key) ?value) ...))
    ((_ ?class ?key ?value ?thing ...)
     (make-instance ?class (:slots (?key . ?value)) ?thing ...))
    ((_ ?class)
     (make-instance ?class '()))))

(define <this>
  '((:slots . (:a :b :c :d))))

(make-instance <this> :a 1)
(make-instance <this> :a 1 :c 3)
(make-instance <this> :a 1 :c 3 :b 2)
(make-instance <this> :a 1 :d 4 :c 3 :b 2)

;;; end of file

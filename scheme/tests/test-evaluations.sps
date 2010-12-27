;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for evaluations library
;;;Date: Thu Dec 16, 2010
;;;
;;;Abstract
;;;
;;;
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
(import (nausicaa)
  (nausicaa evaluations)
  (rnrs eval)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing evaluations\n")


(parametrise ((check-test-name	'values))

  (check
      (let (((e <environment>) (make <environment>)))
	(receive ()	;no return values
	    (e.eval '(values))
	  #t))
    => #t)

  (check
      (let (((e <environment>) (make <environment>)))
	(e.eval '(+ 1 2)))
    => 3)

  (check
      (let (((e <environment>) (make <environment>)))
	(receive (a b c)
	    (e.eval '(values 1 2 3))
	  (list a b c)))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'imports))

  (check
      (let (((e <environment>) (make <environment>
				 (imports: '((only (rnrs) + - * /))))))
	(e.eval '(+ 1 (- 2 (* 3 (/ 4 5))))))
    => 3/5)
;;;(pretty-print (+ 1 (- 2 (* 3 (/ 4 5)))))

  (check	;null environment
      (let* ((f (lambda (x) (+ 1 x)))
	     ((e <environment>)	(make <environment>
				  (bindings: `((f . ,f)))
				  (imports:  '()))))
	(e.eval '(f 2)))
    => 3)

  #t)


(parametrise ((check-test-name	'bindings))

  (check	;generating a binding
      (let (((e <environment>) (make <environment>)))
	(e.eval-for-bindings '(begin (define a 1))
			     '(a)))
    => '((a . 1)))

  (check	;generating a binding and using it
      (let* (((p <environment>) (make <environment>))
	     (binds		(p.eval-for-bindings '(begin (define a 2))
						     '(a)))
	     ((q <environment>)	(make <environment>
				  (bindings: binds))))
	(q.eval '(begin a)))
    => 2)

  (check	;using an external binding
      (let* ((f			(lambda (x)
				  (+ 1 x)))
	     ((e <environment>) (make <environment>
				  (bindings: `((f . ,f))))))
	(e.eval '(f 2)))
    => 3)

  (check	;augmented environment
      (let* (((p <environment>)	(make <environment>
				  (bindings: '((a . 1)
					       (b . 2)))))
	     ((q <environment>)	(p.augment '((c . 3)
					     (d . 4)))))
	(q.eval '(list a b c d)))
    => '(1 2 3 4))

  (check	;self augmenting bindings
      (let (((e <environment>)	(make <environment>
				  (bindings: '((a . 1)
					       (b . 2))))))
	(e.eval-to-augment! '(begin
			       (define c 3)
			       (define d 4))
			    '(c d))
	(e.eval '(list a b c d)))
    => '(1 2 3 4))

;;; --------------------------------------------------------------------
;;; errors

  (check	;augmenting environment causes duplication
      (guard (E ((assertion-violation? E)
		 #t)
		(else E))
	(eval '(let (((p <environment>)	(make <environment>
					  (bindings: '((a . 1)
						       (b . 2))))))
		 (p.augment '((c . 3)
			      (b . 4)))
		 #f)
	      (environment '(nausicaa) '(nausicaa evaluations))))
    => #t)

  (check	;self augmenting environment causes duplication
      (guard (E ((assertion-violation? E)
		 #t)
		(else E))
	(let (((e <environment>) (make <environment>
				   (bindings: '((a . 1)
						(b . 2))))))
	  (e.eval-to-augment! '(begin
				 (define c 3)
				 (define b 4))
			      '(c b))))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file

;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for generic functions as class methods
;;;Date: Tue Mar 22, 2011
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (nausicaa)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing generic functions as classe methods\n")


(parametrise ((check-test-name	'method-redefinition))

  (define-class <alpha>
    (fields a)
    (methods (doit doit)))

  (define-class <beta>
    (inherit <alpha>)
    (fields b)
    (methods (doit doit)))

  (define-generic doit)

  (define-method (doit (o <alpha>) arg)
    (list 'doit-alpha arg))

  (define-method (doit (o <beta>) arg)
    (list 'doit-beta arg))

  (check
      (let (((o <alpha>) (make <alpha> 1)))
	(o.doit 10))
    => '(doit-alpha 10))

  (check
      (let (((o <beta>) (make <beta> 1 2)))
	(o.doit 20))
    => '(doit-beta 20))

  (check
      (let (((o <alpha>) (make <beta> 1 2)))
	(o.doit 30))
    => '(doit-beta 30))

  #t)


(parametrise ((check-test-name	'pre-post-conditions))

  (define-class <alpha>
    (fields a)
    (methods (doit doit)))

  (define-class <beta>
    (inherit <alpha>)
    (fields b)
    (methods (doit doit)))

  (define-generic* doit)

  (define-method (doit (o <alpha>) arg)
    (list 'doit-alpha arg))

  (define-method (doit (o <beta>) arg)
    (list 'doit-beta arg))

  (define-method doit :before ((o <alpha>) arg)
    (add-result 'before-alpha))

  (define-method doit :after ((o <alpha>) arg)
    (add-result 'after-alpha))

  (define-method doit :before ((o <beta>) arg)
    (add-result 'before-beta))

  (define-method doit :after ((o <beta>) arg)
    (add-result 'after-beta))

  (check
      (with-result
       (let (((o <alpha>) (make <beta> 1 2)))
	 (o.doit 30)))
    => '((doit-beta 30)
	 (before-beta before-alpha after-alpha after-beta)))

  #t)


;;;; done

(check-report)

;;; end of file

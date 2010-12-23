;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for nausicaa mutable pairs
;;;Date: Thu Dec 23, 2010
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
  (nausicaa mutable-pairs)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing nausicaa mutable pairs\n")


(parametrise ((check-test-name	'mutation))

  (check
      (let ((p (cons 1 2)))
        (set-car! p 3)
	p)
    => '(3 . 2))

  (check
      (let ((p (cons 1 2)))
        (set-cdr! p 3)
	p)
    => '(1 . 3))

  #t)


(parametrise ((check-test-name	'label))

  (check
      (let-make ((p <mutable-pair> 1 2))
	(is-a? p <mutable-pair>))
    => #t)

  (check
      (is-a? 123 <mutable-pair>)
    => #f)

  (check
      (let-make ((p <mutable-pair> 1 2))
        p.car)
    => 1)

  (check
      (let-make ((p <mutable-pair> 1 2))
        p.cdr)
    => 2)

  (check
      (let-make ((p <mutable-pair> 1 2))
        (set! p.car 3)
	p)
    => '(3 . 2))

  (check
      (let-make ((p <mutable-pair> 1 2))
        (set! p.cdr 3)
	p)
    => '(1 . 3))

  #t)


;;;; done

(check-report)

;;; end of file

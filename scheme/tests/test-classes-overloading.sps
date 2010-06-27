;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for method overloading with classes library
;;;Date: Sun Jun 27, 2010
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


(import (nausicaa)
  (generics)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing classes method overloading\n")


(parametrise ((check-test-name	'basic))

  (define-class <alpha>
    (fields a)
    (methods doit))

  (define-generic <alpha>-doit)

  (define-method (<alpha>-doit (o <alpha>) (v <char>))
    (cons 'char v))

  (define-method (<alpha>-doit (o <alpha>) (v <integer>))
    (cons 'int  v))

  (check
      (let (((o <alpha>) (make <alpha> 1)))
	(o.doit #\a))
    => '(char . #\a))

  (check
      (let (((o <alpha>) (make <alpha> 1)))
	(o.doit 2))
    => '(int . 2))

  #t)


;;;; done

(check-report)

;;; end of file

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
  (evaluations)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing evaluations\n")


(parametrise ((check-test-name	'values))

  (check
      (let (((e <environment>) (make <environment>)))
        (receive (binds)
	    (e.eval '(values))
	  binds))
    => '())

  (check
      (let (((e <environment>) (make <environment>)))
  	(receive (binds result)
  	    (e.eval '(+ 1 2))
  	  (list binds result)))
    => '(() 3))

  (check
      (let (((e <environment>) (make <environment>)))
        (receive (binds a b c)
  	    (e.eval '(values 1 2 3))
  	  (list binds a b c)))
    => '(() 1 2 3))

  #t)


;;;; done

(check-report)

;;; end of file

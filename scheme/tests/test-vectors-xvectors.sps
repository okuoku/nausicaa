;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for label interface to vector functions
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
  (vectors xvectors)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing xvectors\n")


(parametrise ((check-test-name	'constructors))

  (check
      (let (((o <xvector>) '#(0 1 2 3)))
	(o.append '#(4 5 6 7 8)))
    => '#(0 1 2 3 4 5 6 7 8))

  #t)


(parameterise ((check-test-name 'concatenate))

  (check
      (let (((o <xvector>) '#(#\c #\i #\a #\o)))
	(o.concatenate '(#(#\space)
			 #(#\h #\e #\l #\l #\o) #(#\space)
			 #(#\s #\a #\l #\u #\t))))
    => '#(#\c #\i #\a #\o #\space #\h #\e #\l #\l #\o #\space #\s #\a #\l #\u #\t))

  #t)


;;;; done

(check-report)

;;; end of file

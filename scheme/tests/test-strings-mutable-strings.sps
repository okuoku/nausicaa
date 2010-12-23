;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for mutable string label
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
  (nausicaa mutable-strings)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing mutable string label\n")


(parametrise ((check-test-name	'inheritance))

  (check
      (let (((S <mutable-string>) "ciao"))
        S.length)
    => 4)

  (check
      (let (((S <mutable-string>) (make <mutable-string> #\c #\i #\a #\o)))
        S.upcase)
    => "CIAO")

  (check
      (let (((S <mutable-string>) "ciao"))
        (getf (S 2)))
    => #\a)

  #t)


(parametrise ((check-test-name	'mutable))

  (check
      (is-a? "ciao" <mutable-string>)
    => #t)

  (check
      (is-a? 123 <mutable-string>)
    => #f)

  (check
      (let (((S <mutable-string>) (string-copy "ciao")))
	(setf (S 2) #\W)
        (getf (S 2)))
    => #\W)

  #t)


;;;; done

(check-report)

;;; end of file

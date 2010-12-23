;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for xstring label
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
  (strings xstrings)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing xstring label\n")


(parametrise ((check-test-name	'inheritance))

  (check
      (let (((S <xstring>) "ciao"))
        (S.append " mamma"))
    => "ciao mamma")

  #t)


(parametrise ((check-test-name	'constructors))

  (check
      (let (((S <xstring>) "ciao"))
        (S.concatenate '(" mamma")))
    => "ciao mamma")

  #t)


;;;; done

(check-report)

;;; end of file

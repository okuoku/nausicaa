;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for years and weeks manipulations
;;;Date: Mon Jul 19, 2010
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
  (times-and-dates years-and-weeks)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing times and dates, years and weeks\n")


(parametrise ((check-test-name	'years))

  (check (%gregorian-leap-year? 0)		=> #t)
  (check (%gregorian-leap-year? 4)		=> #t)
  (check (%gregorian-leap-year? 1)		=> #f)

  (check (%gregorian-leap-year? 1600)		=> #t)
  (check (%gregorian-leap-year? 2000)		=> #t)
  (check (%gregorian-leap-year? 2008)		=> #t)
  (check (%gregorian-leap-year? 2400)		=> #t)
  (check (%gregorian-leap-year? 2800)		=> #t)

  (check (%gregorian-leap-year? 1700)		=> #f)
  (check (%gregorian-leap-year? 1800)		=> #f)
  (check (%gregorian-leap-year? 1900)		=> #f)

;;; --------------------------------------------------------------------

  (check
      (%gregorian-year-number-of-days-since-beginning 2000 1 1)
    => 1)

  (check
      (%gregorian-year-number-of-days-since-beginning 2000 2 1)
    => 32)

  (check
      (%gregorian-year-number-of-days-since-beginning 2000 3 1)
    => (+ 31 29 1))

  (check
      (%gregorian-year-number-of-days-since-beginning 2001 3 1)
    => (+ 31 28 1))

;;; --------------------------------------------------------------------

  (check
      (gregorian-natural-year 0 1900)
    => 1900)

  (check
      (gregorian-natural-year 10 1900)
    => 1910)

  (check
      (gregorian-natural-year 90 1900)
    => 2000)

  (check
      (gregorian-natural-year 50 1900)
    => 1950)

  (check
      (gregorian-natural-year 51 1900)
    => 2000)

  ;;
  ;;   		%easter-month-and-day


  #t)


;;;; done

(check-report)

;;; end of file

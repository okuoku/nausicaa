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

  (check (gregorian-leap-year? 0)		=> #t)
  (check (gregorian-leap-year? 4)		=> #t)
  (check (gregorian-leap-year? 1)		=> #f)

  (check (gregorian-leap-year? 1600)		=> #t)
  (check (gregorian-leap-year? 2000)		=> #t)
  (check (gregorian-leap-year? 2008)		=> #t)
  (check (gregorian-leap-year? 2400)		=> #t)
  (check (gregorian-leap-year? 2800)		=> #t)

  (check (gregorian-leap-year? 1700)		=> #f)
  (check (gregorian-leap-year? 1800)		=> #f)
  (check (gregorian-leap-year? 1900)		=> #f)

;;; --------------------------------------------------------------------

  (check
      (gregorian-year-number-of-days-since-beginning 2000 1 1)
    => 1)

  (check
      (gregorian-year-number-of-days-since-beginning 2000 2 1)
    => 32)

  (check
      (gregorian-year-number-of-days-since-beginning 2000 3 1)
    => (+ 31 29 1))

  (check
      (gregorian-year-number-of-days-since-beginning 2001 3 1)
    => (+ 31 28 1))

;;; --------------------------------------------------------------------

  (check (gregorian-natural-year 10 1903) => 1910)
  (check (gregorian-natural-year 10 1913) => 1910)
  (check (gregorian-natural-year 10 1923) => 1910)
  (check (gregorian-natural-year 10 1933) => 1910)
  (check (gregorian-natural-year 10 1943) => 1910)
  (check (gregorian-natural-year 10 1953) => 1910)
  (check (gregorian-natural-year 10 1963) => 2010)
  (check (gregorian-natural-year 10 1973) => 2010)
  (check (gregorian-natural-year 10 1983) => 2010)
  (check (gregorian-natural-year 10 1993) => 2010)
  (check (gregorian-natural-year 10 2003) => 2010)

  (check (gregorian-natural-year 50 1903) => 1950)
  (check (gregorian-natural-year 50 1913) => 1950)
  (check (gregorian-natural-year 50 1923) => 1950)
  (check (gregorian-natural-year 50 1933) => 1950)
  (check (gregorian-natural-year 50 1943) => 1950)
  (check (gregorian-natural-year 50 1953) => 1950)
  (check (gregorian-natural-year 50 1963) => 1950)
  (check (gregorian-natural-year 50 1973) => 1950)
  (check (gregorian-natural-year 50 1983) => 1950)
  (check (gregorian-natural-year 50 1993) => 1950)
  (check (gregorian-natural-year 50 2003) => 2050)

  (check (gregorian-natural-year 90 1903) => 1890)
  (check (gregorian-natural-year 90 1913) => 1890)
  (check (gregorian-natural-year 90 1923) => 1890)
  (check (gregorian-natural-year 90 1933) => 1890)
  (check (gregorian-natural-year 90 1943) => 1990)
  (check (gregorian-natural-year 90 1953) => 1990)
  (check (gregorian-natural-year 90 1963) => 1990)
  (check (gregorian-natural-year 90 1973) => 1990)
  (check (gregorian-natural-year 90 1983) => 1990)
  (check (gregorian-natural-year 90 1993) => 1990)
  (check (gregorian-natural-year 90 2003) => 1990)

;;; --------------------------------------------------------------------

  (let-syntax ((lov (syntax-rules ()
		      ((_ . ?body)
		       (call-with-values (lambda () . ?body) list)))))

    (check (lov (gregorian-year-western-easter-month-and-day 1982))	=> '(4 11))
    (check (lov (gregorian-year-western-easter-month-and-day 1983))	=> '(4 3))
    (check (lov (gregorian-year-western-easter-month-and-day 1984))	=> '(4 22))
    (check (lov (gregorian-year-western-easter-month-and-day 1985))	=> '(4 7))
    (check (lov (gregorian-year-western-easter-month-and-day 1986))	=> '(3 30))
    (check (lov (gregorian-year-western-easter-month-and-day 1987))	=> '(4 19))
    (check (lov (gregorian-year-western-easter-month-and-day 1988))	=> '(4 3))
    (check (lov (gregorian-year-western-easter-month-and-day 1989))	=> '(3 26))
    (check (lov (gregorian-year-western-easter-month-and-day 1990))	=> '(4 15))
    (check (lov (gregorian-year-western-easter-month-and-day 1991))	=> '(3 31))
    (check (lov (gregorian-year-western-easter-month-and-day 1992))	=> '(4 19))
    (check (lov (gregorian-year-western-easter-month-and-day 1993))	=> '(4 11))
    (check (lov (gregorian-year-western-easter-month-and-day 1994))	=> '(4 3))
    (check (lov (gregorian-year-western-easter-month-and-day 1995))	=> '(4 16))
    (check (lov (gregorian-year-western-easter-month-and-day 1996))	=> '(4 7))
    (check (lov (gregorian-year-western-easter-month-and-day 1997))	=> '(3 30))
    (check (lov (gregorian-year-western-easter-month-and-day 1998))	=> '(4 12))
    (check (lov (gregorian-year-western-easter-month-and-day 1999))	=> '(4 4))
    (check (lov (gregorian-year-western-easter-month-and-day 2000))	=> '(4 23))
    (check (lov (gregorian-year-western-easter-month-and-day 2001))	=> '(4 15))
    (check (lov (gregorian-year-western-easter-month-and-day 2002))	=> '(3 31))
    (check (lov (gregorian-year-western-easter-month-and-day 2003))	=> '(4 20))
    (check (lov (gregorian-year-western-easter-month-and-day 2004))	=> '(4 11))
    (check (lov (gregorian-year-western-easter-month-and-day 2005))	=> '(3 27))
    (check (lov (gregorian-year-western-easter-month-and-day 2006))	=> '(4 16))
    (check (lov (gregorian-year-western-easter-month-and-day 2007))	=> '(4 8))
    (check (lov (gregorian-year-western-easter-month-and-day 2008))	=> '(3 23))
    (check (lov (gregorian-year-western-easter-month-and-day 2009))	=> '(4 12))
    (check (lov (gregorian-year-western-easter-month-and-day 2010))	=> '(4 4))
    (check (lov (gregorian-year-western-easter-month-and-day 2011))	=> '(4 24))
    (check (lov (gregorian-year-western-easter-month-and-day 2012))	=> '(4 8))
    (check (lov (gregorian-year-western-easter-month-and-day 2013))	=> '(3 31))
    (check (lov (gregorian-year-western-easter-month-and-day 2014))	=> '(4 20))
    (check (lov (gregorian-year-western-easter-month-and-day 2015))	=> '(4 5))
    (check (lov (gregorian-year-western-easter-month-and-day 2016))	=> '(3 27))
    (check (lov (gregorian-year-western-easter-month-and-day 2017))	=> '(4 16))
    (check (lov (gregorian-year-western-easter-month-and-day 2018))	=> '(4 1))
    (check (lov (gregorian-year-western-easter-month-and-day 2019))	=> '(4 21))
    (check (lov (gregorian-year-western-easter-month-and-day 2020))	=> '(4 12))
    (check (lov (gregorian-year-western-easter-month-and-day 2021))	=> '(4 4))
    (check (lov (gregorian-year-western-easter-month-and-day 2022))	=> '(4 17))

    #f)

  #t)


(parametrise ((check-test-name	'weeks))

  (check (gregorian-index-of-day-in-week 2010 01 01)	=> 5)
  (check (gregorian-index-of-day-in-week 2010 01 02)	=> 6)
  (check (gregorian-index-of-day-in-week 2010 01 03)	=> 0)
  (check (gregorian-index-of-day-in-week 2010 01 04)	=> 1)
  (check (gregorian-index-of-day-in-week 2010 01 05)	=> 2)
  (check (gregorian-index-of-day-in-week 2010 01 06)	=> 3)
  (check (gregorian-index-of-day-in-week 2010 01 07)	=> 4)

  (check (gregorian-index-of-day-in-week 2000 02 27)	=> 0)
  (check (gregorian-index-of-day-in-week 2000 02 28)	=> 1)
  (check (gregorian-index-of-day-in-week 2000 02 29)	=> 2)
  (check (gregorian-index-of-day-in-week 2000 03 01)	=> 3)
  (check (gregorian-index-of-day-in-week 2000 03 02)	=> 4)
  (check (gregorian-index-of-day-in-week 2000 03 03)	=> 5)
  (check (gregorian-index-of-day-in-week 2000 03 04)	=> 6)

;;; --------------------------------------------------------------------

;;;       January 2010
;;;   Su Mo Tu We Th Fr Sa
;;;                   1  2
;;;    3  4  5  6  7  8  9
;;;   10 11 12 13 14 15 16
;;;   17 18 19 20 21 22 23
;;;   24 25 26 27 28 29 30
;;;   31

;;;   0  1  2  3  4  5  6

  (check (gregorian-number-of-days-before-first-week 2010 5)	=> 0)
  (check (gregorian-number-of-days-before-first-week 2010 6)	=> 1)
  (check (gregorian-number-of-days-before-first-week 2010 0)	=> 2)
  (check (gregorian-number-of-days-before-first-week 2010 1)	=> 3)
  (check (gregorian-number-of-days-before-first-week 2010 2)	=> 4)
  (check (gregorian-number-of-days-before-first-week 2010 3)	=> 5)
  (check (gregorian-number-of-days-before-first-week 2010 4)	=> 6)

  #t)


;;;; done

(check-report)

;;; end of file

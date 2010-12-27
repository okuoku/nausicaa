;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for times and dates value types
;;;Date: Wed Sep 29, 2010
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
  (nausicaa times-and-dates types)
  (rnrs eval)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing times-and-dates types\n")


(parametrise ((check-test-name	'types))

  (check (years-type? 1900) => #t)
  (check (years-type? "1900") => #f)

  (check (month-type? 12) => #t)
  (check (month-type? #\a) => #f)

  (check (day-type? 12) => #t)
  (check (day-type? #\a) => #f)

  (check (hours-type? 12) => #t)
  (check (hours-type? #\a) => #f)

  (check (minutes-type? 12) => #t)
  (check (minutes-type? #\a) => #f)

  (check (seconds-type? 12) => #t)
  (check (seconds-type? #\a) => #f)

  (check (milliseconds-type? 12) => #t)
  (check (milliseconds-type? #\a) => #f)

  (check (microseconds-type? 12) => #t)
  (check (microseconds-type? #\a) => #f)

  (check (nanoseconds-type? 12) => #t)
  (check (nanoseconds-type? #\a) => #f)

  #t)


(parametrise ((check-test-name	'types-assert))

  (define-syntax check-type
    (syntax-rules ()
      ((_ ?func ?ok ?bad)
       (begin
	 (check
	     (?func 'dummy ?ok)
	   => #t)
	 (check
	     (guard (E ((assertion-violation? E)
			#t)
		       (else #f))
	       (eval '(?func 'dummy ?ok)
		     (environment '(rnrs)
				  '(times-and-dates types))))
	   => #t)
	 ))))

;;; --------------------------------------------------------------------

  (check-type assert-years-type		1900	#\a)
  (check-type assert-month-type		12	#\a)
  (check-type assert-day-type		12	#\a)
  (check-type assert-hours-type		12	#\a)
  (check-type assert-minutes-type	12	#\a)
  (check-type assert-seconds-type	12	#\a)
  (check-type assert-milliseconds-type	12	#\a)
  (check-type assert-microseconds-type	12	#\a)
  (check-type assert-nanoseconds-type	12	#\a)

  #t)


(parametrise ((check-test-name	'ranges))

  (check (date-years-range? 10)		=> #t)
  (check (date-years-range? -10)	=> #t)
  (check (date-years-range? 0)		=> #f)

  (check (date-month-range? 12)		=> #t)
  (check (date-month-range? 10)		=> #t)
  (check (date-month-range? 1)		=> #t)
  (check (date-month-range? 0)		=> #f)
  (check (date-month-range? 13)		=> #f)

  (check (date-day-range? 12 1)		=> #t)
  (check (date-day-range?  1 1)		=> #t)
  (check (date-day-range? 31 1)		=> #t)
  (check (date-day-range? 28 2)		=> #t)
  (check (date-day-range?  0 1)		=> #f)
  (check (date-day-range? 32 1)		=> #f)
  (check (date-day-range? 29 2)		=> #f)

  (check (date-day-range?/leap-year 12 1)	=> #t)
  (check (date-day-range?/leap-year  1 1)	=> #t)
  (check (date-day-range?/leap-year 31 1)	=> #t)
  (check (date-day-range?/leap-year 29 2)	=> #t)
  (check (date-day-range?/leap-year  0 1)	=> #f)
  (check (date-day-range?/leap-year 32 1)	=> #f)
  (check (date-day-range?/leap-year 30 2)	=> #f)

  (check (date-hours-range? 0)		=> #t)
  (check (date-hours-range? 10)		=> #t)
  (check (date-hours-range? 23)		=> #t)
  (check (date-hours-range? -1)		=> #f)
  (check (date-hours-range? 24)		=> #f)

  (check (date-minutes-range? 0)	=> #t)
  (check (date-minutes-range? 10)	=> #t)
  (check (date-minutes-range? 59)	=> #t)
  (check (date-minutes-range? -1)	=> #f)
  (check (date-minutes-range? 60)	=> #f)

  (check (date-seconds-range? 0)	=> #t)
  (check (date-seconds-range? 10)	=> #t)
  (check (date-seconds-range? 59)	=> #t)
  (check (date-seconds-range? -1)	=> #f)
  (check (date-seconds-range? 60)	=> #f)

  (check (date-seconds-range?/with-leap 0)	=> #t)
  (check (date-seconds-range?/with-leap 10)	=> #t)
  (check (date-seconds-range?/with-leap 59)	=> #t)
  (check (date-seconds-range?/with-leap 60)	=> #t)
  (check (date-seconds-range?/with-leap -1)	=> #f)
  (check (date-seconds-range?/with-leap 61)	=> #f)

  (check (date-milliseconds-range? 0)		=> #t)
  (check (date-milliseconds-range? 999)		=> #t)
  (check (date-milliseconds-range? -1)		=> #f)
  (check (date-milliseconds-range? 1000)	=> #f)

  (check (date-microseconds-range? 0)		=> #t)
  (check (date-microseconds-range? 999)		=> #t)
  (check (date-microseconds-range? -1)		=> #f)
  (check (date-microseconds-range? 1000)	=> #f)

  (check (date-nanoseconds-range? 0)		=> #t)
  (check (date-nanoseconds-range? 999)		=> #t)
  (check (date-nanoseconds-range? -1)		=> #f)
  (check (date-nanoseconds-range? 1000)		=> #f)

  #t)


(parametrise ((check-test-name	'ranges))

  (define-syntax check-range
    (syntax-rules ()
      ((_ ?func (?ok ...) (?bad ...))
       (begin
	 (check
	     (?func 'dummy ?ok)
	   => #t)
	 ...
	 (check
	     (guard (E ((assertion-violation? E)
;;;			(write E)(newline)
			'good)
		       (else 'bad))
	       (eval '(?func 'dummy ?bad)
		     (environment '(rnrs)
				  '(times-and-dates types))))
	   => 'good)
	 ...
	 ))))

;;; --------------------------------------------------------------------

  (check-range assert-date-years-range			(10 -10)	(0))
  (check-range assert-date-month-range			(12 10 1)	(0 13))
  (check-range assert-date-hours-range			(0 10 23)	(-1 24))
  (check-range assert-date-minutes-range		(0 10 59)	(-1 60))
  (check-range assert-date-seconds-range		(0 10 59)	(-1 60))
  (check-range assert-date-seconds-range/with-leap	(0 10 59 60)	(-1 61))
  (check-range assert-date-milliseconds-range		(0 999)		(-1 1000))
  (check-range assert-date-microseconds-range		(0 999)		(-1 1000))
  (check-range assert-date-nanoseconds-range		(0 999)		(-1 1000))

  (check-range (lambda (who day)
		 (assert-date-day-range who day 1))
	       (1 12 31) (0 32))

  (check-range (lambda (who day)
		 (assert-date-day-range who day 2))
	       (1 12 28) (0 29))

  (check-range (lambda (who day)
		 (assert-date-day-range/leap-year who day 1))
	       (12 31 28) (0 32))

  (check-range (lambda (who day)
		 (assert-date-day-range/leap-year who day 2))
	       (1 12 29) (0 30))

  #t)


;;;; done

(check-report)

;;; end of file

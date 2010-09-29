;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: predicates and assertions for time and date values
;;;Date: Mon Sep 27, 2010
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
(library (times-and-dates types)
  (export

    ;; type predicates
    years-type?
    month-type?
    day-type?
    hours-type?
    minutes-type?
    seconds-type?
    milliseconds-type?
    microseconds-type?
    nanoseconds-type?

    ;; range predicates
    date-years-range?
    date-month-range?
    date-day-range?
    date-day-range?/leap-year
    date-hours-range?
    date-minutes-range?
    date-seconds-range?
    date-seconds-range?/with-leap
    date-milliseconds-range?
    date-microseconds-range?
    date-nanoseconds-range?

    ;; type assertions
    assert-years-type
    assert-month-type
    assert-day-type
    assert-hours-type
    assert-minutes-type
    assert-seconds-type
    assert-milliseconds-type
    assert-microseconds-type
    assert-nanoseconds-type

    ;; range assertions
    assert-date-years-range
    assert-date-month-range
    assert-date-day-range
    assert-date-day-range/leap-year
    assert-date-hours-range
    assert-date-minutes-range
    assert-date-seconds-range
    assert-date-seconds-range/with-leap
    assert-date-milliseconds-range
    assert-date-microseconds-range
    assert-date-nanoseconds-range
    )
  (import (rnrs)
    (only (language-extensions) define-inline)
    (type-utilities))


;;;; type predicates

(define-inline (years-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))

(define-inline (month-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))

(define-inline (day-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))

(define-inline (hours-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))

(define-inline (minutes-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))

(define-inline (seconds-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))

(define-inline (milliseconds-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))

(define-inline (microseconds-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))

(define-inline (nanoseconds-type? obj)
  (and (number? obj) (exact? obj) (integer? obj)))


;;;; type assertions

(define-type-assertion years-type
  (predicate		years-type?)
  (type-description	"exact integer")
  (value-description	"count of years"))

(define-type-assertion month-type
  (predicate		month-type?)
  (type-description	"exact integer")
  (value-description	"month index"))

(define-type-assertion day-type
  (predicate		month-type?)
  (type-description	"exact integer")
  (value-description	"day index"))

(define-type-assertion hours-type
  (predicate		hours-type?)
  (type-description	"exact integer")
  (value-description	"count of hours"))

(define-type-assertion minutes-type
  (predicate		minutes-type?)
  (type-description	"exact integer")
  (value-description	"count of minutes"))

(define-type-assertion seconds-type
  (predicate		seconds-type?)
  (type-description	"exact integer")
  (value-description	"count of seconds"))

(define-type-assertion milliseconds-type
  (predicate		milliseconds-type?)
  (type-description	"exact integer")
  (value-description	"count of milliseconds"))

(define-type-assertion microseconds-type
  (predicate		microseconds-type?)
  (type-description	"exact integer")
  (value-description	"count of microseconds"))

(define-type-assertion nanoseconds-type
  (predicate		nanoseconds-type?)
  (type-description	"exact integer")
  (value-description	"count of nanoseconds"))


;;;; range predicates

(define-inline (date-years-range? obj)
  (not (zero? obj)))

(define-inline (date-month-range? obj)
  (<= 1 obj 12))

(define-inline (date-day-range? obj month-index)
  (case month-index
    ((1 3 5 7 8 10 12)
     (<= 1 obj 31))
    ((4 6 9 11)
     (<= 1 obj 30))
    ((2)
     (<= 1 obj 28))
    (else #f)))

(define-inline (date-day-range?/leap-year obj month-index)
  (case month-index
    ((1 3 5 7 8 10 12)
     (<= 1 obj 31))
    ((4 6 9 11)
     (<= 1 obj 30))
    ((2)
     (<= 1 obj 29))
    (else #f)))

(define-inline (date-hours-range? obj)
  (<= 0 obj 23))

(define-inline (date-minutes-range? obj)
  (<= 0 obj 59))

(define-inline (date-seconds-range? obj)
  (<= 0 obj 59))
(define-inline (date-seconds-range?/with-leap obj)
  (<= 0 obj 60))

(define-inline (date-milliseconds-range? obj)
  (<= 0 obj 999))

(define-inline (date-microseconds-range? obj)
  (<= 0 obj 999))

(define-inline (date-nanoseconds-range? obj)
  (<= 0 obj 999))


;;;; range assertions

(define-type-assertion date-years-range
  (predicate		date-years-range?)
  (type-description	"non-zero number")
  (value-description	"count of years in date"))

(define-type-assertion date-month-range
  (predicate		date-month-range?)
  (type-description	"index between 1 and 12")
  (value-description	"month index in date"))

(define-inline (assert-date-day-range who obj month-index)
  (if (date-day-range? obj month-index)
      #t
    (assertion-violation who
      (string-append "expected adequate day index for month index "
		     (number->string month-index) " in date")
      obj)))

(define-inline (assert-date-day-range/leap-year who obj month-index)
  (if (date-day-range?/leap-year obj month-index)
      #t
    (assertion-violation who
      (string-append "expected adequate day index for month index "
		     (number->string month-index) " in date with leap year")
      obj)))

(define-type-assertion date-hours-range
  (predicate		date-hours-range?)
  (type-description	"number between 0 and 23")
  (value-description	"count of hours in date"))

(define-type-assertion date-minutes-range
  (predicate		date-minutes-range?)
  (type-description	"number between 0 and 59")
  (value-description	"count of minutes in date"))

(define-type-assertion date-seconds-range
  (predicate		date-seconds-range?)
  (type-description	"number between 0 and 59")
  (value-description	"count of seconds in date"))

(define-type-assertion date-seconds-range/with-leap
  (predicate		date-seconds-range?/with-leap)
  (type-description	"number between 0 and 60")
  (value-description	"count of seconds (including leap) in date"))

(define-type-assertion date-milliseconds-range
  (predicate		date-milliseconds-range?)
  (type-description	"number between 0 and 999")
  (value-description	"count of milliseconds in date"))

(define-type-assertion date-microseconds-range
  (predicate		date-microseconds-range?)
  (type-description	"number between 0 and 999")
  (value-description	"count of microseconds in date"))

(define-type-assertion date-nanoseconds-range
  (predicate		date-nanoseconds-range?)
  (type-description	"number between 0 and 999")
  (value-description	"count of nanoseconds in date"))


;;;; done

)

;;; end of file

;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for parser-tools libraries
;;;Date: Tue Sep  8, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (checks)
  (records)
  (parser-tools lexical-token)
  (parser-tools source-location))

(check-set-mode! 'report-failed)
(display "*** testing parser-tools\n")


(parametrise ((check-test-name 'predicates))

  (check
      (<source-location>? (make-<source-location>/start "this"))
    => #t)

  (check
      (<source-location>? #f)
    => #f)

  (check
      (<source-location>? 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (<source-location>?/or-false (make-<source-location>/start "this"))
    => #t)

  (check
      (<source-location>?/or-false #f)
    => #t)

  (check
      (<source-location>?/or-false 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (<source-location>?/start (make-<source-location> "this" 1 2 3))
    => #f)

  (check
      (<source-location>?/start (make-<source-location>/start "this"))
    => #t)

  (check
      (<source-location>?/start #f)
    => #f)

  (check
      (<source-location>?/start 123)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (<source-location>?/start/or-false (make-<source-location> "this" 1 2 3))
    => #f)

  #t)


(parametrise ((check-test-name 'comparison))

  (let ((a (make-<source-location> #f 1 2 3))
  	(b (make-<source-location> #f 1 2 3)))

    (check (source-location=? a b)		=> #t)
    (check (source-location-point=? a b)	=> #t)
    (check (source-location-point<? a b)	=> #f)
    (check (source-location-point>? a b)	=> #f)
    (check (source-location-point<=? a b)	=> #t)
    (check (source-location-point>=? a b)	=> #t)

    #f)

  (let ((a1 (make-<source-location> #f 1 2 3))
  	(b1 (make-<source-location> #f 1 2 30)))

    (check (source-location=? a1 b1)		=> #f)
    (check (source-location-point=? a1 b1)	=> #t)
    (check (source-location-point<? a1 b1)	=> #f)
    (check (source-location-point>? a1 b1)	=> #f)
    (check (source-location-point<=? a1 b1)	=> #t)
    (check (source-location-point>=? a1 b1)	=> #t)

    #f)

  (let ((a2 (make-<source-location> #f 1 2 3))
  	(b2 (make-<source-location> #f 10 2 3)))

    (check (source-location=? a2 b2)		=> #f)
    (check (source-location-point=? a2 b2)	=> #f)
    (check (source-location-point<? a2 b2)	=> #t)
    (check (source-location-point>? a2 b2)	=> #f)
    (check (source-location-point<=? a2 b2)	=> #t)
    (check (source-location-point>=? a2 b2)	=> #f)

    #f)

  (let ((a3 (make-<source-location> #f 1 2 3))
	(b3 (make-<source-location> #f 1 20 3)))

    (check (source-location=? a3 b3)		=> #f)
    (check (source-location-point=? a3 b3)	=> #f)
    (check (source-location-point<? a3 b3)	=> #t)
    (check (source-location-point>? a3 b3)	=> #f)
    (check (source-location-point<=? a3 b3)	=> #t)
    (check (source-location-point>=? a3 b3)	=> #f)

    #f)

  #t)


(parametrise ((check-test-name 'update))

  (check	;token length
      (let ((loc (make-<source-location> 'this 10 20 30)))
	(source-location-update loc 4))
    (=> source-location=?)
    (make-<source-location> 'this 10 (+ 20 4) (+ 30 4)))

  (check	;newline char
      (let ((loc (make-<source-location> 'this 10 20 30)))
	(source-location-update loc #\newline))
    (=> source-location=?)
    (make-<source-location> 'this (+ 10 1) 1 (+ 30 1)))

  (check	;return char
      (let ((loc (make-<source-location> 'this 10 20 30)))
	(source-location-update loc #\return))
    (=> source-location=?)
    (make-<source-location> 'this 10 (+ 20 1) (+ 30 1)))

  (check	;return char
      (parametrise ((source-location-honor-return #f))
	(let ((loc (make-<source-location> 'this 10 20 30)))
	  (source-location-update loc #\return)))
    (=> source-location=?)
    (make-<source-location> 'this 10 (+ 20 1) (+ 30 1)))

  (check	;return char
      (parametrise ((source-location-honor-return #t))
	(let ((loc (make-<source-location> 'this 10 20 30)))
	  (source-location-update loc #\return)))
    (=> source-location=?)
    (make-<source-location> 'this 10 1 (+ 30 1)))

  (check	;tab char, 8chars
      (let ((loc (make-<source-location> 'this 10 20 30)))
	(source-location-update loc #\tab))
    (=> source-location=?)
    (make-<source-location> 'this 10 24 (+ 30 1)))

  (check	;tab char, 8chars
      (parametrise ((source-location-tab-function source-location-tab-function/8chars))
	(let ((loc (make-<source-location> 'this 10 20 30)))
	  (source-location-update loc #\tab)))
    (=> source-location=?)
    (make-<source-location> 'this 10 24 (+ 30 1)))

  (check	;tab char, table function
      (parametrise ((source-location-tab-function source-location-tab-function/tab-table))
	(let ((loc (make-<source-location> 'this 10 20 30)))
	  (source-location-update loc #\tab)))
    (=> source-location=?)
    (make-<source-location> 'this 10 24 (+ 30 1)))

  (check	;tab char, table function, tab table
      (parametrise ((source-location-tab-function	source-location-tab-function/tab-table)
		    (source-location-tab-table		'(6 12 18 24 30 36)))
	(let ((loc (make-<source-location> 'this 10 20 30)))
	  (source-location-update loc #\tab)))
    (=> source-location=?)
    (make-<source-location> 'this 10 24 (+ 30 1)))

  (check	;tab char, table function, short tab table
      (parametrise ((source-location-tab-function	source-location-tab-function/tab-table)
		    (source-location-tab-table		'(6 12 18)))
	(let ((loc (make-<source-location> 'this 10 20 30)))
	  (source-location-update loc #\tab)))
    (=> source-location=?)
    (make-<source-location> 'this 10 24 (+ 30 1)))

  #t)


(parametrise ((check-test-name 'misc))

  (check
      (object->string (make-<source-location> 'this 10 20 30))
    => "this:10:20")

  #t)


;;;; done

(check-report)

;;; end of file

;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for foreign classes
;;;Date: Thu Dec 23, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing foreign classes\n")


(parametrise ((check-test-name	'strings))

  (check
      (is-a? "ciao" <string>)
    => #t)

  (check
      (is-a? 123 <string>)
    => #f)

  (check
      (let (((o <string>) (string-copy "ciao")))
        o.length)
    => 4)

  (check
      (let (((o <string>) (string-copy "ciao")))
        o.empty?)
    => #f)

  (check
      (let (((o <string>) ""))
        o.empty?)
    => #t)

  (check
      (let (((o <string>) (string-copy "ciao")))
        o.upcase)
    => "CIAO")

  (check
      (let (((o <string>) (string-copy "ciAO")))
        o.downcase)
    => "ciao")

  (check
      (let (((o <string>) (string-copy "ciAO")))
        o.titlecase)
    => "Ciao")

;;; --------------------------------------------------------------------

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o.substring 2))
    => "ao")

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o.substring 2 3))
    => "a")

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o.append " mamma"))
    => "ciao mamma")

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o.list))
    => '(#\c #\i #\a #\o))

  (check
      (let (((o <string>) (string-copy "ciao")))
        (o.copy))
    => "ciao")

  (check
      (let (((o <string>) (string-copy "ciao"))
	    (result '()))
        (o.for-each (lambda (ch)
		      (set! result (cons ch result))))
	result)
    => (reverse '(#\c #\i #\a #\o)))

  #t)


(parametrise ((check-test-name	'lists))

  (check
      (is-a? '(1 2 3) <list>)
    => #t)

  (check
      (is-a? '() <list>)
    => #t)

  (check
      (is-a? 123 <list>)
    => #f)

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o.find (lambda (n) (= 3 n))))
    => 3)

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o.for-all (lambda (n) (< 0 n))))
    => #t)

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o.exists (lambda (n) (if (= 3 n) n #f))))
    => 3)

  #t)


;;;; done

(check-report)

;;; end of file

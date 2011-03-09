;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for lexical token records
;;;Date: Fri Oct 29, 2010
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
  (nausicaa parser-tools lexical-token)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing parser tools's lexical token\n")


(parametrise ((check-test-name	'makers))

  (check
      (let (((T <lexical-token>) (make* <lexical-token>
				   'category 'location 'value 5)))
	(list T.category T.location T.value T.length))
    => '(category location value 5))

;;; --------------------------------------------------------------------

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category: 'woppa))))
	(list T.category T.location T.value T.length))
    => '(woppa #f #f 0))

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category:	'category)
				   (location:	'location)
				   (value:	'value)
				   (length:	5))))
	(list T.category T.location T.value T.length))
    => '(category location value 5))

  #t)


(parametrise ((check-test-name	'predicates))

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category: '*eoi*))))
	T.end-of-input?)
    => #t)

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category: 'woppa))))
	T.end-of-input?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category: '*lexer-error*))))
	T.lexer-error?)
    => #t)

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category: 'woppa))))
	T.lexer-error?)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category: '*eoi*))))
	T.special?)
    => #t)

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category: '*lexer-error*))))
	T.special?)
    => #t)

  (check
      (let (((T <lexical-token>) (make <lexical-token>
				   (category: 'woppa))))
	T.special?)
    => #f)

  #t)


;;;; done

(check-report)

;;; end of file

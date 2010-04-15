;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for library names library
;;;Date: Thu Apr 15, 2010
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
  (libraries names)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing library names\n")


(parametrise ((check-test-name	'decomposition))

  (check
      (library-name->version '(alpha))
    => '())

  (check
      (library-name->version '(alpha beta))
    => '())

  (check
      (library-name->version '(alpha beta gamma))
    => '())

  (check
      (library-name->version '(alpha beta gamma ()))
    => '())

  (check
      (library-name->version '(alpha beta gamma (1)))
    => '(1))

  (check
      (library-name->version '(alpha beta gamma (1 2 3)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (library-name->identifiers '(alpha))
    => '(alpha))

  (check
      (library-name->identifiers '(alpha beta))
    => '(alpha beta))

  (check
      (library-name->identifiers '(alpha beta gamma))
    => '(alpha beta gamma))

  (check
      (library-name->identifiers '(alpha beta gamma ()))
    => '(alpha beta gamma))

  (check
      (library-name->identifiers '(alpha beta gamma (1)))
    => '(alpha beta gamma))

  (check
      (library-name->identifiers '(alpha beta gamma (1 2 3)))
    => '(alpha beta gamma))

;;; --------------------------------------------------------------------

  (let-syntax ((check-decomposition
		(syntax-rules ()
		  ((_ ?sexp ?result)
		   (check
		       (receive (identifiers version)
			   (library-name-decompose ?sexp)
			 (vector identifiers version))
		     => ?result)))))

    (check-decomposition '(alpha)
			 '#( (alpha) () ))

    (check-decomposition '(alpha beta)
			 '#( (alpha beta) () ))

    (check-decomposition '(alpha beta gamma)
			 '#( (alpha beta gamma) () ))

    (check-decomposition '(alpha beta gamma ())
			 '#( (alpha beta gamma) () ))

    (check-decomposition '(alpha beta gamma (1))
			 '#( (alpha beta gamma) (1) ))

    (check-decomposition '(alpha beta gamma (1 2 3))
			 '#( (alpha beta gamma) (1 2 3) ))

    #f)
  #f)


(parametrise ((check-test-name	'predicates))

  (check
      (library-name? '())
    => #f)

  (check
      (library-name? '(alpha))
    => #t)

  (check
      (library-name? '(alpha beta))
    => #t)

  (check
      (library-name? '(alpha beta gamma))
    => #t)

  (check
      (library-name? '(alpha beta gamma ()))
    => #t)

  (check
      (library-name? '(alpha beta gamma (1)))
    => #t)

  (check
      (library-name? '(alpha beta gamma (1 2 3)))
    => #t)

  (check
      (library-name? '(alpha 123 gamma))
    => #f)

  (check
      (library-name? '(alpha beta gamma (1 ciao)))
    => #f)

  #t)


(parametrise ((check-test-name	'predicates))

  (check (library-version=? '() '()) => #t)
  (check (library-version=? '(1) '(1)) => #t)
  (check (library-version=? '(1 2) '(1 2)) => #t)
  (check (library-version=? '(1 2 3) '(1 2 3)) => #t)

  (check (library-version=? '(1) '(2)) => #f)
  (check (library-version=? '(1 2) '(1 3)) => #f)
  (check (library-version=? '(1 2 3) '(1 2 4)) => #f)

  (check (library-version=? '(1 2 0 0 0) '(1 2)) => #t)
  (check (library-version=? '(1 2) '(1 2 0 0 0)) => #t)

  (check (library-version=? '(1 2 0 0 1) '(1 2)) => #f)
  (check (library-version=? '(1 2) '(1 2 0 0 1)) => #f)

;;; --------------------------------------------------------------------

  (check (library-version<? '() '()) => #f)
  (check (library-version<? '(1) '(1)) => #f)
  (check (library-version<? '(1 2) '(1 2)) => #f)
  (check (library-version<? '(1 2 3) '(1 2 3)) => #f)

  (check (library-version<? '(1) '(2)) => #t)
  (check (library-version<? '(2) '(1)) => #f)
  (check (library-version<? '(1 2) '(1 3)) => #t)
  (check (library-version<? '(1 3) '(1 2)) => #f)
  (check (library-version<? '(1 2 3) '(1 2 4)) => #t)
  (check (library-version<? '(1 2 4) '(1 2 3)) => #f)

  (check (library-version<? '(1 2 0 0 0) '(1 2)) => #f)
  (check (library-version<? '(1 2) '(1 2 0 0 0)) => #f)

  (check (library-version<? '(1 2 0 0 1) '(1 2)) => #f)
  (check (library-version<? '(1 2) '(1 2 0 0 1)) => #t)

;;; --------------------------------------------------------------------

  (check (library-version<=? '() '()) => #t)
  (check (library-version<=? '(1) '(1)) => #t)
  (check (library-version<=? '(1 2) '(1 2)) => #t)
  (check (library-version<=? '(1 2 3) '(1 2 3)) => #t)

  (check (library-version<=? '(1) '(2)) => #t)
  (check (library-version<=? '(2) '(1)) => #f)
  (check (library-version<=? '(1 2) '(1 3)) => #t)
  (check (library-version<=? '(1 3) '(1 2)) => #f)
  (check (library-version<=? '(1 2 3) '(1 2 4)) => #t)
  (check (library-version<=? '(1 2 4) '(1 2 3)) => #f)

  (check (library-version<=? '(1 2 0 0 0) '(1 2)) => #t)
  (check (library-version<=? '(1 2) '(1 2 0 0 0)) => #t)

  (check (library-version<=? '(1 2 0 0 1) '(1 2)) => #f)
  (check (library-version<=? '(1 2) '(1 2 0 0 1)) => #t)

  #t)


;;;; done

(check-report)

;;; end of file

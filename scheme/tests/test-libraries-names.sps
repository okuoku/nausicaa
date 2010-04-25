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


(parametrise ((check-test-name	'comparison))

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

;;; --------------------------------------------------------------------

  (check (library-name-identifiers=? '(a) '(a)) => #t)
  (check (library-name-identifiers=? '(a b) '(a b)) => #t)
  (check (library-name-identifiers=? '(a b c) '(a b c)) => #t)

  (check (library-name-identifiers=? '(a) '(b)) => #f)
  (check (library-name-identifiers=? '(a b) '(b a)) => #f)
  (check (library-name-identifiers=? '(a b c) '(a b z)) => #f)

  (check (library-name-identifiers=? '(a b c (2)) '(a b c (1))) => #t)
  (check (library-name-identifiers=? '(a b c (2)) '(a b d (1))) => #f)

;;; --------------------------------------------------------------------

  (check (library-name=? '(a) '(a)) => #t)
  (check (library-name=? '(a b) '(a b)) => #t)
  (check (library-name=? '(a b c) '(a b c)) => #t)

  (check (library-name=? '(a) '(b)) => #f)
  (check (library-name=? '(a b) '(b a)) => #f)
  (check (library-name=? '(a b c) '(a b z)) => #f)

  (check (library-name=? '(a b c (2)) '(a b c (1))) => #f)
  (check (library-name=? '(a b c (2)) '(a b d (1))) => #f)

  (check (library-name=? '(a ()) '(a ())) => #t)
  (check (library-name=? '(a (1)) '(a (1))) => #t)
  (check (library-name=? '(a (1 2)) '(a (1 2))) => #t)
  (check (library-name=? '(a (1 2 3)) '(a (1 2 3))) => #t)

  (check (library-name=? '(a (1)) '(a (2))) => #f)
  (check (library-name=? '(a (1 2)) '(a (1 3))) => #f)
  (check (library-name=? '(a (1 2 3)) '(a (1 2 4))) => #f)

  (check (library-name=? '(a (1 2 0 0 0)) '(a (1 2))) => #t)
  (check (library-name=? '(a (1 2)) '(a (1 2 0 0 0))) => #t)

  (check (library-name=? '(a (1 2 0 0 1)) '(a (1 2))) => #f)
  (check (library-name=? '(a (1 2)) '(a (1 2 0 0 1))) => #f)

;;; --------------------------------------------------------------------

  (check (library-name<? '(a ()) '(a ())) => #f)
  (check (library-name<? '(a (1)) '(a (1))) => #f)
  (check (library-name<? '(a (1 2)) '(a (1 2))) => #f)
  (check (library-name<? '(a (1 2 3)) '(a (1 2 3))) => #f)

  (check (library-name<? '(a (1)) '(a (2))) => #t)
  (check (library-name<? '(a (2)) '(a (1))) => #f)
  (check (library-name<? '(a (1 2)) '(a (1 3))) => #t)
  (check (library-name<? '(a (1 3)) '(a (1 2))) => #f)
  (check (library-name<? '(a (1 2 3)) '(a (1 2 4))) => #t)
  (check (library-name<? '(a (1 2 4)) '(a (1 2 3))) => #f)

  (check (library-name<? '(a (1 2 0 0 0)) '(a (1 2))) => #f)
  (check (library-name<? '(a (1 2)) '(a (1 2 0 0 0))) => #f)

  (check (library-name<? '(a (1 2 0 0 1)) '(a (1 2))) => #f)
  (check (library-name<? '(a (1 2)) '(a (1 2 0 0 1))) => #t)

;;; --------------------------------------------------------------------

  (check (library-name<=? '(a ()) '(a ())) => #t)
  (check (library-name<=? '(a (1)) '(a (1))) => #t)
  (check (library-name<=? '(a (1 2)) '(a (1 2))) => #t)
  (check (library-name<=? '(a (1 2 3)) '(a (1 2 3))) => #t)

  (check (library-name<=? '(a (1)) '(a (2))) => #t)
  (check (library-name<=? '(a (2)) '(a (1))) => #f)
  (check (library-name<=? '(a (1 2)) '(a (1 3))) => #t)
  (check (library-name<=? '(a (1 3)) '(a (1 2))) => #f)
  (check (library-name<=? '(a (1 2 3)) '(a (1 2 4))) => #t)
  (check (library-name<=? '(a (1 2 4)) '(a (1 2 3))) => #f)

  (check (library-name<=? '(a (1 2 0 0 0)) '(a (1 2))) => #t)
  (check (library-name<=? '(a (1 2)) '(a (1 2 0 0 0))) => #t)

  (check (library-name<=? '(a (1 2 0 0 1)) '(a (1 2))) => #f)
  (check (library-name<=? '(a (1 2)) '(a (1 2 0 0 1))) => #t)

  #t)


(parametrise ((check-test-name	'sorting))

  (let ((a (make-<library-name> '(x y (1 2))))
	(b (make-<library-name> '(x y (1 3))))
	(c (make-<library-name> '(x y (1 4))))
	(d (make-<library-name> '(x y (2 1)))))

    (check
	(list-sort (lambda ((a <library-name>) (b <library-name>))
		     (a.< b))
		   (list a b))
      => (list a b))

    (check
	(list-sort (lambda ((a <library-name>) (b <library-name>))
		     (a.< b))
		   (list a d c b))
      => (list a b c d))

    #f)
  #t)


;;;; done

(check-report)

;;; end of file

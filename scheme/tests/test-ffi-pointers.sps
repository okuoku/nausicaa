;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for low level memory pointer functions
;;;Date: Tue Dec 16, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (foreign memory))

(check-set-mode! 'report-failed)
(display "*** testing memory pointers\n")

(cond-expand (petite (exit)) (else #f))


(parametrise ((check-test-name 'null))

  (check
      (pointer? pointer-null)
    => #t)

  (check
      (pointer-null? pointer-null)
    => #t)

  (check
      (pointer-null? (integer->pointer 123))
    => #f)

  (check
      (pointer-null? (integer->pointer 0))
    => #t)

  #t)


(parametrise ((check-test-name 'integers))

  (check
      (pointer? 123)
    => #f)

  (check
      (pointer? (integer->pointer 123))
    => #t)

  (check
      (pointer->integer (integer->pointer 123))
    => 123)

  (check
      (pointer->integer (integer->pointer 0))
    => 0)

  #t)


(parametrise ((check-test-name 'arithmetics))

  (check
      (pointer-diff (integer->pointer 123) (integer->pointer 120))
    => 3)

  (check
      (pointer-diff (integer->pointer 118) (integer->pointer 120))
    => -2)

;;; --------------------------------------------------------------------

  (check
      (pointer-add (integer->pointer 100) 23)
    (=> pointer=?) (integer->pointer 123))

  (check
      (pointer-add (integer->pointer 100) -23)
    (=> pointer=?) (integer->pointer 77))

  #t)


(parametrise ((check-test-name 'comparison))

  (check
      (pointer=? (integer->pointer 123))
    => #t)

  (check
      (pointer=? (integer->pointer 123)
		 (integer->pointer 123))
    => #t)

  (check
      (pointer=? (integer->pointer 123)
		 (integer->pointer 123)
		 (integer->pointer 123))
    => #t)

  (check
      (pointer=? (integer->pointer 123)
		 (integer->pointer 456)
		 (integer->pointer 456))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer<? (integer->pointer 123))
    => #t)

  (check
      (pointer<? (integer->pointer 123)
		 (integer->pointer 123))
    => #f)

  (check
      (pointer<? (integer->pointer 123)
		 (integer->pointer 456)
		 (integer->pointer 789))
    => #t)

  (check
      (pointer<? (integer->pointer 123)
		 (integer->pointer 456)
		 (integer->pointer 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer>? (integer->pointer 123))
    => #t)

  (check
      (pointer>? (integer->pointer 123)
		 (integer->pointer 123))
    => #f)

  (check
      (pointer>? (integer->pointer 789)
		 (integer->pointer 456)
		 (integer->pointer 123))
    => #t)

  (check
      (pointer>? (integer->pointer 123)
		 (integer->pointer 456)
		 (integer->pointer 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer<=? (integer->pointer 123))
    => #t)

  (check
      (pointer<=? (integer->pointer 123)
		  (integer->pointer 123))
    => #t)

  (check
      (pointer<=? (integer->pointer 123)
		  (integer->pointer 456)
		  (integer->pointer 789))
    => #t)

  (check
      (pointer<=? (integer->pointer 123)
		  (integer->pointer 456)
		  (integer->pointer 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer>=? (integer->pointer 123))
    => #t)

  (check
      (pointer>=? (integer->pointer 123)
		  (integer->pointer 123))
    => #t)

  (check
      (pointer>=? (integer->pointer 789)
		  (integer->pointer 456)
		  (integer->pointer 123))
    => #t)

  (check
      (pointer>=? (integer->pointer 123)
		  (integer->pointer 456)
		  (integer->pointer 1))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (pointer<>? (integer->pointer 123))
    => #f)

  (check
      (pointer<>? (integer->pointer 123)
		  (integer->pointer 123))
    => #f)

  (check
      (pointer<>? (integer->pointer 789)
		  (integer->pointer 456)
		  (integer->pointer 123))
    => #t)

  (check
      (pointer<>? (integer->pointer 123)
		  (integer->pointer 456)
		  (integer->pointer 1))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file

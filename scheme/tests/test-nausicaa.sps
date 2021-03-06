;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: test for R6RS compliance and extensions
;;;Date: Mon Dec  8, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(display "*** testing some R6RS compliance and Nausicaa extensions\n")


(parametrise ((check-test-name	'numbers))

  (define epsilon 1e-6)
  (define (eq=? a b)
    ;;This is not  the best definition of equality  between floating point
    ;;numbers.
    ;;
    (let ((ra (real-part a))
	  (rb (real-part b))
	  (ia (imag-part a))
	  (ib (imag-part b)))
      (and (or (= ra rb)
	       (and (nan? ra) (nan? rb))
	       (and (zero? ra) (zero? rb))
	       (< (abs (- ra rb)) epsilon)
	       (< (abs (/ (- ra rb) ra)) epsilon))
	   (or (= ia ib)
	       (and (nan? ia) (nan? ib))
	       (and (zero? ia) (zero? ib))
	       (< (abs (- ia ib)) epsilon)
	       (< (abs (/ (- ia ib) ia)) epsilon)))))

  (check
      (integer? (equal-hash '#(1 2 3)))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (finite? 123)
    => #t)

  (check
      (finite? +inf.0)
    => #f)

  (check
      (finite? 123+456i)
    => #t)

  (check
      (finite? +inf.0+456i)
    => #f)

  (check
      (finite? 123+inf.0i)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (infinite? 123)
    => #f)

  (check
      (infinite? +inf.0)
    => #t)

  (check
      (infinite? 123+456i)
    => #f)

  (check
      (infinite? +inf.0+456i)
    => #t)

  (check
      (infinite? 123+inf.0i)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (nan? 123)
    => #f)

  (check
      (nan? +inf.0)
    => #f)

  (check
      (nan? 123+456i)
    => #f)

  (check
      (nan? +inf.0+456i)
    => #f)

  (check
      (nan? 123+inf.0i)
    => #f)

  (check
      (nan? +nan.0)
    => #t)

  (check
      (nan? 123+nan.0i)
    => #t)

  (check
      (nan? +nan.0+456i)
    => #t)

;;; --------------------------------------------------------------------
  ;;
  ;;It goes like this: all the  real numbers (according to REAL?) are also
  ;;rational  numbers  (according  to  RATIONAL?) with  the  exception  of
  ;;infinities  and NaNs.   Exactness does  NOT matter  for  the RATIONAL?
  ;;predicate.
  ;;

  (check (rational? 1)		=> #t)
  (check (rational? #i1)		=> #t)
  (check (rational? #e1)		=> #t)
  (check (rational? 1.2)		=> #t)
  (check (rational? #e1.2)	=> #t)
  (check (rational? #i1.2)	=> #t)
  (check (rational? 12/10)	=> #t)
  (check (rational? #e12/10)	=> #t)
  (check (rational? #i12/10)	=> #t)
  (check (rational? +nan.0)	=> #f)
  (check (rational? -nan.0)	=> #f)
  (check (rational? +inf.0)	=> #f)
  (check (rational? -inf.0)	=> #f)

;;; --------------------------------------------------------------------
  ;;
  ;;It goes like  this: all the rational numbers  (according to RATIONAL?)
  ;;are also integer numbers (according to INTEGER?) if the denominator is
  ;;"=" to  1.  Notice that exactness  does NOT matter  for the comparison
  ;;function  "=",   so  exactness  does  not  matter   for  the  INTEGER?
  ;;predicate, too.
  ;;

  (check (integer? 1)		=> #t)
  (check (integer? #i1)		=> #t)
  (check (integer? #e1)		=> #t)
  (check (integer? 1.0)		=> #t)
  (check (integer? #i1.0)		=> #t)
  (check (integer? #e1.0)		=> #t)
  (check (integer? 1.2)		=> #f)
  (check (integer? #e1.2)		=> #f)
  (check (integer? #i1.2)		=> #f)
  (check (integer? 12/10)		=> #f)
  (check (integer? #e12/10)	=> #f)
  (check (integer? #i12/10)	=> #f)
  (check (integer? +nan.0)	=> #f)
  (check (integer? -nan.0)	=> #f)
  (check (integer? +inf.0)	=> #f)
  (check (integer? -inf.0)	=> #f)

;;; --------------------------------------------------------------------
  ;;
  ;;The -VALUED? predicates are like  the normal predicate, but the return
  ;;#t also on complex numbers  whose imaginary part is zero (according to
  ;;ZERO?).  Notice that ZERO? does not care about exactness.
  ;;

  (check (rational-valued? 1)		=> #t)
  (check (rational-valued? #i1)		=> #t)
  (check (rational-valued? #e1)		=> #t)
  (check (rational-valued? 1.0)		=> #t)
  (check (rational-valued? #i1.0)		=> #t)
  (check (rational-valued? #e1.0)		=> #t)
  (check (rational-valued? 1.2)		=> #t)
  (check (rational-valued? #e1.2)		=> #t)
  (check (rational-valued? #i1.2)		=> #t)
  (check (rational-valued? 12/10)		=> #t)
  (check (rational-valued? #e12/10)	=> #t)
  (check (rational-valued? #i12/10)	=> #t)
  (check (rational-valued? +nan.0)	=> #f)
  (check (rational-valued? -nan.0)	=> #f)
  (check (rational-valued? +inf.0)	=> #f)
  (check (rational-valued? -inf.0)	=> #f)

  (check (rational-valued? 1+2i)		=> #f)
  (check (rational-valued? 1+2.2i)	=> #f)
  (check (rational-valued? 1.1+2i)	=> #f)
  (check (rational-valued? 1.1+2.2i)	=> #f)
  (check (rational-valued? 1+0i)		=> #t)
  (check (rational-valued? 1+0.0i)	=> #t)
  (check (rational-valued? 1.1+0i)	=> #t)
  (check (rational-valued? 1.1+0.0i)	=> #t)
  (check (rational-valued? (+ +nan.0 0+0.0i))	=> #f)
  (check (rational-valued? (+ +nan.0 0+0i))	=> #f)

  (check (real-valued? 1)			=> #t)
  (check (real-valued? #i1)		=> #t)
  (check (real-valued? #e1)		=> #t)
  (check (real-valued? 1.0)		=> #t)
  (check (real-valued? #i1.0)		=> #t)
  (check (real-valued? #e1.0)		=> #t)
  (check (real-valued? 1.2)		=> #t)
  (check (real-valued? #e1.2)		=> #t)
  (check (real-valued? #i1.2)		=> #t)
  (check (real-valued? 12/10)		=> #t)
  (check (real-valued? #e12/10)		=> #t)
  (check (real-valued? #i12/10)		=> #t)
  (check (real-valued? +nan.0)		=> #t)
  (check (real-valued? -nan.0)		=> #t)
  (check (real-valued? +inf.0)		=> #t)
  (check (real-valued? -inf.0)		=> #t)

  (check (real-valued? 1+2i)		=> #f)
  (check (real-valued? 1+2.2i)		=> #f)
  (check (real-valued? 1.1+2i)		=> #f)
  (check (real-valued? 1.1+2.2i)		=> #f)
  (check (real-valued? 1+0i)		=> #t)
  (check (real-valued? 1+0.0i)		=> #t)
  (check (real-valued? 1.1+0i)		=> #t)
  (check (real-valued? 1.1+0.0i)		=> #t)
  (check (real-valued? (+ +nan.0 0+0.0i))	=> #t)
  (check (real-valued? (+ +nan.0 0+0i))	=> #t)

;;; --------------------------------------------------------------------

  (check (equal? 1 2) => #f)
  (check (equal? 1 1) => #t)

  (check (equal? +nan.0 +nan.0)	=> #t)
  (check (equal? +nan.0 -nan.0)	=> #t)
  (check (equal? -nan.0 +nan.0)	=> #t)
  (check (equal? -nan.0 -nan.0)	=> #t)

  (check (equal? +nan.0 (string->number "+nan.0"))	=> #t)
  (check (equal? +nan.0 (string->number "-nan.0"))	=> #t)
  (check (equal? -nan.0 (string->number "+nan.0"))	=> #t)
  (check (equal? -nan.0 (string->number "-nan.0"))	=> #t)

  (check (eqv? +nan.0 +nan.0)	=> #t)
  (check (eqv? +nan.0 -nan.0)	=> #t)
  (check (eqv? -nan.0 +nan.0)	=> #t)
  (check (eqv? -nan.0 -nan.0)	=> #t)

  (check (eqv? +nan.0 (string->number "+nan.0"))	=> #t)
  (check (eqv? +nan.0 (string->number "-nan.0"))	=> #t)
  (check (eqv? -nan.0 (string->number "+nan.0"))	=> #t)
  (check (eqv? -nan.0 (string->number "-nan.0"))	=> #t)

;;; --------------------------------------------------------------------

  (check-for-true (nan? (max 1 +nan.0)))
  (check-for-true (nan? (min 1 +nan.0)))
  (check-for-true (nan? (* 0 +nan.0)))

;;; --------------------------------------------------------------------

  (check (=) => #t)

  (check (= 1) => #t)

  (check (= 1 2) => #f)

  (check (= 1 1) => #t)

;;; --------------------------------------------------------------------

  (check (expt +inf.0+2.i 2)	(=> eq=?) +inf.0+nan.0i)

  #t)


;;;; done

(check-report)

;;; end of file

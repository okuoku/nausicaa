;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: tests for parameters
;;;Date: Thu Dec 25, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(import (r6rs)
  (lang-lib)
  (check-lib))

(check-set-mode! 'report-failed)
(display "*** testing parameters\n")



;;;; code

(define alpha
  (make-parameter 123))

(define beta
  (make-parameter 0
    (lambda (num)
      (unless (number? num)
	(raise 'woppa))
      num)))

;;; --------------------------------------------------------------------

(check
    (alpha)
  => 123)

(check
    (parameterize ((alpha 456))
      (list (alpha)))
  => '(456))

(check
    (parameterize ((alpha 456))
      (list (alpha)
	    (parameterize ((alpha 789))
	      (alpha))))
  => '(456 789))

;;; --------------------------------------------------------------------

(check
    (beta)
  => 0)

(check
    (parameterize ((beta 456))
      (list (beta)))
  => '(456))

(check
    (parameterize ((beta 456))
      (list (beta)
	    (parameterize ((beta 789))
	      (beta))))
  => '(456 789))

(check
    (guard (exc (else exc))
      (parameterize ((beta 'b))
	(list (beta))))
  => 'woppa)

;;; --------------------------------------------------------------------

(define gamma
  (make-parameter 0
    (lambda (num)
      (unless (number? num)
	(raise 'woppa))
      (case num
	((0) 'zero)
	((1) 'one)
	((2) 'two)))))

(check
    (gamma)
  => 'zero)

(check
    (begin
      (gamma 1)
      (gamma))
  => 'one)

(check
    (begin
      (gamma 2)
      (gamma))
  => 'two)


;;;; done

(check-report)

;;; end of file

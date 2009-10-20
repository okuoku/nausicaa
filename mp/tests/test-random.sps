;;;
;;;Part of: Nausicaa/MP
;;;Contents: tests for the pseudo-random numbers API
;;;Date: Tue Oct 20, 2009
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
  (compensations)
  (checks)
  (foreign memory)
  (foreign math mp random)
  (foreign math mp sizeof))

(check-set-mode! 'report-failed)
(display "*** testing random\n")


(parametrise ((check-test-name 'integers))

  (with-compensations
    (letrec ((state (compensate
			(malloc sizeof-gmp_randstate_t)
		      (with
		       (gmp_randclear state)
		       (primitive-free state)))))
      (gmp_randinit_default state)
      (gmp_randseed_ui state 123)

      (check
	  (integer? (gmp_urandomb_ui state 10))
	=> #t)

      (check
	  (let ((n (gmp_urandomm_ui state 10)))
	    (and (integer? n)
		 (<= 0 n)
		 (<  n 10)))
	=> #t)

      #f))
  #t)


;;;; done

(check-report)

;;; end of file

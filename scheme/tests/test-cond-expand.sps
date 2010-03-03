;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for cond-expand
;;;Date: Mon Dec 29, 2008
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
  (rnrs eval (6))
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing cond-expand\n")


;;;; code

(define-syntax check-feature
  (syntax-rules ()
    ((_ ?feature)
     (check
	 (cond-expand (?feature	#t)
		      (else	#f))
       => #t))))

(check
    (cond-expand
     (ikarus	#t)
     (larceny	#t)
     (mosh	#t)
     (ypsilon	#t)
     (else	#f))
  => #t)

(check
    (cond-expand
     (woppa	123)
     (wippa	456)
     (else	#t))
  => #t)

(check
    (cond-expand
     ((not woppa)	123)
     (wippa		456)
     (else		#t))
  => 123)

(check
    (cond-expand
     ((or r6rs woppa)	123)
     (wippa		456)
     (else		#t))
  => 123)

(check
    (cond-expand
     ((or woppa r6rs)	123)
     (wippa		456)
     (else		#t))
  => 123)

(check
    (cond-expand
     ((and woppa r6rs)	123)
     (wippa		456)
     (else		#t))
  => #t)

(check
    (cond-expand
     ((and r6rs woppa)	123)
     (wippa		456)
     (else		#t))
  => #t)

(check
    (cond-expand
     ((and r6rs r6rs)
      123)
     (wippa		456)
     (else		#t))
  => 123)

(check
    (guard (exc (else (syntax-violation? exc)))
      (eval '(cond-expand
	      (woppa	#f))
	    (environment '(nausicaa))))
  => #t)



;;;; done

(check-report)

;;; end of file

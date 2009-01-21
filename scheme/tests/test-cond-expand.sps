;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for cond-expand
;;;Date: Mon Dec 29, 2008
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

(import (scheme)
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

(check-feature and-let*)
(check-feature args-fold)
(check-feature char-set)
(check-feature checks)
(check-feature compare)
(check-feature cut)
(check-feature format)
(check-feature lists)
(check-feature loops)
(check-feature parameters)
(check-feature random)
(check-feature receive)
(check-feature recursion)
(check-feature sharing)
(check-feature streams)
(check-feature strings)
(check-feature time)
(check-feature vectors)

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
     ((or lists woppa)	123)
     (wippa		456)
     (else		#t))
  => 123)

(check
    (cond-expand
     ((or woppa lists)	123)
     (wippa		456)
     (else		#t))
  => 123)

(check
    (cond-expand
     ((and woppa lists)	123)
     (wippa		456)
     (else		#t))
  => #t)

(check
    (cond-expand
     ((and lists woppa)	123)
     (wippa		456)
     (else		#t))
  => #t)

(check
    (cond-expand
     ((and strings lists)
      123)
     (wippa		456)
     (else		#t))
  => 123)

(check
    (guard (exc (else (syntax-violation? exc)))
      (eval '(cond-expand
	      (woppa	#f))
	    (environment '(scheme))))
  => #t)



;;;; done

(check-report)

;;; end of file

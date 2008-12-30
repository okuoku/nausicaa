;;;
;;;Part of: Nausicaa/SRFI
;;;Contents: tests for cond-expand
;;;Date: Mon Dec 29, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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
  (check-lib)
  (features-lib))

(check-set-mode! 'report-failed)


;;;; code

(define-syntax check-feature
  (syntax-rules ()
    ((_ ?feature)
     (check
	 (cond-expand (?feature	#t)
		      (else	#f))
       => #t))))

(check-feature srfi-0)
(check-feature srfi-1)
(check-feature srfi-2)
(check-feature srfi-6)
(check-feature srfi-8)
(check-feature srfi-9)
(check-feature srfi-13)
(check-feature srfi-14)
(check-feature srfi-19)
(check-feature srfi-26)
(check-feature srfi-27)
(check-feature srfi-31)
(check-feature srfi-37)
(check-feature srfi-38)
(check-feature srfi-39)
(check-feature srfi-41)
(check-feature srfi-42)
(check-feature srfi-43)
(check-feature srfi-48)
(check-feature srfi-61)
(check-feature srfi-67)
(check-feature srfi-78)

(check-feature (srfi cond-expand))
(check-feature (srfi lists))
(check-feature (srfi and-let*))
(check-feature (srfi string-ports))
(check-feature (srfi receive))
(check-feature (srfi records))
(check-feature (srfi strings))
(check-feature (srfi char-set))
(check-feature (srfi time))
(check-feature (srfi cut))
(check-feature (srfi random))
(check-feature (srfi rec))
(check-feature (srfi args-fold))
(check-feature (srfi sharing))
(check-feature (srfi parameters))
(check-feature (srfi streams))
(check-feature (srfi eager-comprehensions))
(check-feature (srfi vectors))
(check-feature (srfi format))
(check-feature (srfi general-cond))
(check-feature (srfi compare))
(check-feature (srfi lightweight-testing))



;;;; done

(check-report)

;;; end of file

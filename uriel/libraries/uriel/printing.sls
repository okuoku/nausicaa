;;;
;;;Part of: Uriel libraries
;;;Contents: printing functions
;;;Date: Mon Nov 24, 2008
;;;Time-stamp: <2008-12-16 10:12:51 marco>
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

(library (uriel printing)
  (export
    print pretty-print)
  (import (r6rs)
    (uriel printing compat))


;;;; code

(define (print port template . args)
  (let ((formatted-string (apply format template args)))
    (when port
      (display formatted-string
	       (cond
		((output-port? port)
		 port)
		((equal? port #t)
		 (current-output-port))
		(else
		 (assertion-violation
		     'print
		   "expected #t or an output port" port)))))
    formatted-string))


;;;; done

)

;;; end of file

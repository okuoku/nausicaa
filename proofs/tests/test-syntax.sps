;;;
;;;Part of: Nausicaa/Proofs
;;;Contents: tests for syntax objects
;;;Date: Fri Nov 28, 2008
;;;Time-stamp: <2008-11-28 09:38:18 marco>
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This  program  is free  software:  you  can redistribute  it
;;;and/or modify it  under the terms of the  GNU General Public
;;;License as published by the Free Software Foundation, either
;;;version  3 of  the License,  or (at  your option)  any later
;;;version.
;;;
;;;This  program is  distributed in  the hope  that it  will be
;;;useful, but  WITHOUT ANY WARRANTY; without  even the implied
;;;warranty  of  MERCHANTABILITY or  FITNESS  FOR A  PARTICULAR
;;;PURPOSE.   See  the  GNU  General Public  License  for  more
;;;details.
;;;
;;;You should  have received a  copy of the GNU  General Public
;;;License   along   with    this   program.    If   not,   see
;;;<http://www.gnu.org/licenses/>.
;;;



;;;; setup

(import (rnrs)
  (uriel printing)
  (uriel test)
  (srfi parameters))

(check-set-mode! 'report-failed)


;;;; syntax objects

(parameterize ((testname 'syntax-object-know))

  (check
      (let ()
	(define (doit stx)
	  (pretty-print stx)
	  #f)

	(let ((alpha 1)
	      (beta  2)
	      (delta 3))
	  (doit (syntax (alpha beta delta)))))
    => #f)


  )


;;;; done

(check-report)

;;; end of file

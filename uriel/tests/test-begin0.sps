;;;
;;;Part of: Uriel libraries for Ikarus Scheme
;;;Contents: tests for begin0
;;;Date: Fri Nov 14, 2008
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



;;; ------------------------------------------------------------
;;; Setup.
;;; ------------------------------------------------------------

(import (rnrs)
  (only (ikarus) printf pretty-print)
  (srfi lightweight-testing)
  (uriel lang))

(check-set-mode! 'report-failed)

;;; ------------------------------------------------------------


;;; ------------------------------------------------------------
;;; Code.
;;; ------------------------------------------------------------

(check
    (begin0
	(list 1 2)
      (list 3 4))
  => '(1 2))

(check
    (call-with-values
	(lambda ()
	  (begin0
	      (values 1 2)
	    (values 3 4)))
      (lambda (a b)
	(list a b)))
  => '(1 2))

;;; ------------------------------------------------------------


;;; ------------------------------------------------------------
;;; Done.
;;; ------------------------------------------------------------

(check-report)

;;; end of file

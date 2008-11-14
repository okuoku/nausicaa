;;;
;;;Part of: Uriel libraries for Ikarus Scheme
;;;Contents: tests for object property
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


;;;page
;;; ------------------------------------------------------------
;;; Setup.
;;; ------------------------------------------------------------

(import (rnrs)
	(uriel object-property)
	(srfi lightweight-testing)
	(only (ikarus) printf parameterize))

(check-set-mode! 'report-failed)

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Code.
;;; ------------------------------------------------------------

(check
    (let ((prop (make-object-property))
	  (a (vector 1 2 3))
	  (b (vector 4 5 6))
	  (c (vector 7 8 9)))
      (prop a 1)
      (prop b 2)
      (list (prop a) (prop b) (prop c)))
  => '(1 2 #f))

(check
    (let ((prop (parameterize ((object-property-initial-capacity 10)
			       (object-property-default-value 'quack))
		  (make-object-property)))
	  (a (vector 1 2 3))
	  (b (vector 4 5 6))
	  (c (vector 7 8 9)))
      (prop a 1)
      (prop b 2)
      (list (prop a) (prop b) (prop c)))
  => '(1 2 quack))

;;; ------------------------------------------------------------

;;;page
;;; ------------------------------------------------------------
;;; Done.
;;; ------------------------------------------------------------

(check-report)

;;; end of file

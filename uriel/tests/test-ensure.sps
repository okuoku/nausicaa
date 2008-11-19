;;;
;;;Part of: Uriel libraries
;;;Contents: tests for ensure
;;;Date: Wed Nov 19, 2008
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



;;; --------------------------------------------------------------------
;;; Setup.
;;; --------------------------------------------------------------------

(import (rnrs)
;;  (only (ikarus) printf pretty-print)
  (srfi lightweight-testing)
  (uriel test)
  (uriel lang))

(check-set-mode! 'report-failed)

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Code.
;;; --------------------------------------------------------------------

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else
	  (add-result 3)
	  (set! flag #t)))))
  => '(#f (1 2 3)))

(check-report)
#!eof

(check
  (lambda ()
    (let ((result	'())
	  (flag		0))
      (ensure (= flag 1)
	  (by
	   (set! result (cons 1 result))
	   (set! result (cons 2 result))
	   (set! flag 1))
	(else
	 (set! result (cons 3 result))
	 (set! result (cons 4 result))))
      result))
  => '(2 1))

;; ------------------------------------------------------------

(check
  (lambda ()
    (let ((result	'())
	  (flag		0))
      (ensure (= flag 1)
	  (by
	   (set! result (cons 1 result))
	   (set! result (cons 2 result)))
	(else-by
	 (set! result (cons 3 result))
	 (set! result (cons 4 result)))
	(else
	 (set! result (cons 5 result))
	 (set! result (cons 6 result))
	 (set! flag 1)))
      result))
  => '(6 5 4 3 2 1))

(check
  (lambda ()
    (let ((result	'())
	  (flag		0))
      (ensure (= flag 1)
	  (by
	   (set! result (cons 1 result))
	   (set! result (cons 2 result)))
	(else-by
	 (set! result (cons 3 result))
	 (set! result (cons 4 result))
	 (set! flag 1))
	(else
	 (set! result (cons 5 result))
	 (set! result (cons 6 result))))
      result))
  => '(4 3 2 1))

(check
  (lambda ()
    (let ((result	'())
	  (flag		0))
      (ensure (= flag 1)
	  (by
	   (set! result (cons 1 result))
	   (set! result (cons 2 result))
	   (set! flag 1))
	(else-by
	 (set! result (cons 3 result))
	 (set! result (cons 4 result)))
	(else
	 (set! result (cons 5 result))
	 (set! result (cons 6 result))))
      result))
  => '(2 1))

;; ------------------------------------------------------------

(check
  (lambda ()
    (let ((result	'())
	  (flag		0))
      (ensure (= flag 1)
	  (by
	   (set! result (cons 1 result))
	   (set! result (cons 2 result)))
	(else-by
	 (set! result (cons 3 result))
	 (set! result (cons 4 result)))
	(else-by
	 (set! result (cons 5 result))
	 (set! result (cons 6 result)))
	(else
	 (set! result (cons 7 result))
	 (set! result (cons 8 result))
	 (set! flag 1)))
      result))
  => '(8 7 6 5 4 3 2 1))

(check
  (lambda ()
    (let ((result	'())
	  (flag		0))
      (ensure (= flag 1)
	  (by
	   (set! result (cons 1 result))
	   (set! result (cons 2 result)))
	(else-by
	 (set! result (cons 3 result))
	 (set! result (cons 4 result)))
	(else-by
	 (set! result (cons 5 result))
	 (set! result (cons 6 result))
	 (set! flag 1))
	(else
	 (set! result (cons 7 result))
	 (set! result (cons 8 result))))
      result))
  => '(6 5 4 3 2 1))

(check
  (lambda ()
    (let ((result	'())
	  (flag		0))
      (ensure (= flag 1)
	  (by
	   (set! result (cons 1 result))
	   (set! result (cons 2 result)))
	(else-by
	 (set! result (cons 3 result))
	 (set! result (cons 4 result))
	 (set! flag 1))
	(else-by
	 (set! result (cons 5 result))
	 (set! result (cons 6 result)))
	(else
	 (set! result (cons 7 result))
	 (set! result (cons 8 result))))
      result))
  => '(4 3 2 1))

(check
  (lambda ()
    (let ((result	'())
	  (flag		0))
      (ensure (= flag 1)
	  (by
	   (set! result (cons 1 result))
	   (set! result (cons 2 result))
	   (set! flag 1))
	(else-by
	 (set! result (cons 3 result))
	 (set! result (cons 4 result)))
	(else-by
	 (set! result (cons 5 result))
	 (set! result (cons 6 result)))
	(else
	 (set! result (cons 7 result))
	 (set! result (cons 8 result))))
      result))
  => '(2 1))

;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------


;;; --------------------------------------------------------------------
;;; Done.
;;; --------------------------------------------------------------------

(check-report)

;;; end of file

;;;
;;;Part of: Uriel libraries
;;;Contents: tests for simple language extensions
;;;Date: Wed Nov 19, 2008
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

(import (rnrs)
  (uriel printing)
  (uriel lang)
  (uriel test))

(check-set-mode! 'report-failed)


;;;; tests for: BEGIN0.

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




;;;; tests for iterators

(check
    (with-result
     (dotimes (i 3)
       1	; shooting the breeze
       2	; shooting the breeze
       (add-result i)))
  => '(#f (0 1 2)))

(check
    (with-result
     (dotimes (i 3 (+ 2 4))
       1	; shooting the breeze
       2	; shooting the breeze
       (add-result i)))
  => '(6 (0 1 2)))

; ------------------------------------------------------------

(check
    (with-result
     (dolist (i '(1 2 3) (+ 2 4))
       1	; shooting the breeze
       2	; shooting the breeze
       (add-result i)))
  => '(6 (1 2 3)))

(check
    (with-result
     (dolist (i '(1 2 3))
       1	; shooting the breeze
       2	; shooting the breeze
       (add-result i)))
  => '(#f (1 2 3)))

; ------------------------------------------------------------

(check
    (with-result
     (loop-upon-list (item '(1 2 3 4))
	 (break-when #f)
       (+ 1 2)	; shooting the breeze
       (+ 3 4)	; shooting the breeze
       (add-result item)))
  => '(#f (1 2 3 4)))

(check
    (with-result
     (loop-upon-list (item '(1 2 3 4) (+ 2 4))
	 (break-when #f)
       (+ 1 2)	; shooting the breeze
       (+ 3 4)	; shooting the breeze
       (add-result item)))
  => '(6 (1 2 3 4)))

(check
    (with-result
     (loop-upon-list (item '(1 2 3 4))
	 (break-when (= item 3))
       (+ 1 2)	; shooting the breeze
       (+ 3 4)	; shooting the breeze
       (add-result item)))
  => '(#f (1 2)))


;;;; done

(check-report)

;;; end of file

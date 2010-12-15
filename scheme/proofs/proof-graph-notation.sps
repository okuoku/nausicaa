;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for graph notation
;;;Date: Mon Dec  1, 2008
;;;
;;;Abstract
;;;
;;;	This  is   to  test  if  all  the   wannahave  supported  Scheme
;;;	implementations implement reading graph notation.
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (rnrs mutable-pairs (6))
  (checks)
  (lists))

(check-set-mode! 'report-failed)
(display "*** testing graph notation\n")



(check
    '#0=(1 . #0#)
    => (let ((v '(1 . #f)))
	 (set-cdr! v v)
	 v))

(check
    '#0=(1 #0#)
    => (let ((v '(1 #f)))
	 (set-car! (cdr v) v)
	 v))

(check
    '#0=(1 2 3 #0#)
    => (let ((v '(1 2 3 #f)))
	 (set-car! (cdddr v) v)
	 v))

(check
    '#0=(#1=(10 #8# 12)
	    #2=(20 21 22)
	    #3=(#5# #4# #3#)
	    #4=(40 #9# 42)
	    #5=(#2# 51 52)
	    #6=(60 61 62)
	    #7=(#1# 71 #2#)
	    #8=(80 81 82)
	    #9=(90 91 #7#)
	    )
    => (let ((v '((10 11 12)
		  (20 21 22)
		  (30 31 32)
		  (40 41 42)
		  (50 51 52)
		  (60 61 62)
		  (70 71 72)
		  (80 81 82)
		  (90 91 92))))

	 ;;#1
	 (set-car! (cdr (first v)) (eighth v))
	 ;;#3
	 (let ((l (third v)))
	   (set-car! l (fifth v))
	   (set-car! (cdr l) (fourth v))
	   (set-car! (cddr l) (third v)))
	 ;;#4
	 (set-car! (cdr (fourth v)) (ninth v))
	 ;;#5
	 (set-car! (fifth v) (second v))
	 ;;#7
	 (let ((l (seventh v)))
	   (set-car! l (first v))
	   (set-car! (cddr l) (second v)))
	 ;;#9
	 (set-car! (cddr (ninth v)) (seventh v))
	 v))


;;;; done

(check-report)

;;; end of file

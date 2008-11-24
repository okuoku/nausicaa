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
  (uriel test)
  (uriel lang))

(check-set-mode! 'report-failed)


;;;; code

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else
	  (add-result 3)))))
  => '(#f (1 2 3)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2)
	    (set! flag 1))
	 (else
	  (add-result 3)
	  (add-result 4)))))
  => '(#f (1 2)))



(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else
	  (add-result 5)
	  (add-result 6)
	  (set! flag 1)))))
  => '(#f (1 2 3 4 5 6)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4)
	  (set! flag 1))
	 (else
	  (add-result 5)
	  (add-result 6)))))
  => '(#f (1 2 3 4)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2)
	    (set! flag 1))
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else
	  (add-result 5)
	  (add-result 6)))))
  => '(#f (1 2)))



(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else-by
	  (add-result 5)
	  (add-result 6))
	 (else
	  (add-result 7)
	  (add-result 8)
	  (set! flag 1)))))
  => '(#f (1 2 3 4 5 6 7 8)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else-by
	  (add-result 5)
	  (add-result 6)
	  (set! flag 1))
	 (else
	  (add-result 7)
	  (add-result 8)))))
  => '(#f (1 2 3 4 5 6)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2))
	 (else-by
	  (add-result 3)
	  (add-result 4)
	  (set! flag 1))
	 (else-by
	  (add-result 5)
	  (add-result 6))
	 (else
	  (add-result 7)
	  (add-result 8)))))
  => '(#f (1 2 3 4)))

(check
    (with-result
     (let ((flag 0))
       (ensure (= flag 1)
	   (by
	    (add-result 1)
	    (add-result 2)
	    (set! flag 1))
	 (else-by
	  (add-result 3)
	  (add-result 4))
	 (else-by
	  (add-result 5)
	  (add-result 6))
	 (else
	  (add-result 7)
	  (add-result 8)))))
  => '(#f (1 2)))


;;;; done

(check-report)

;;; end of file

;;;Part of: Nausicaa/Scheme
;;;Contents: proofs about lists
;;;Date: Tue Jul 28, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (lists)
  (lists low)
  (checks)
  (mosh))

(define-syntax repeat
  (syntax-rules ()
    ((_ ?times ?expr)
     (do ((n 0 (+ 1 n)))
	 ((= n ?times))
       ?expr))))

;;Warm up.
(repeat 1000 (do ((i 0 (+ 1 i))
		  (l (%make-queue 0) (%enqueue! l i)))
		 ((= i 10000)
		  (%queue-list-ref l))))
(repeat 1000 (do ((i 0 (+ 1 i))
		  (l '(0) (cons i l)))
		 ((= i 10000)
		  (reverse l))))

;;The proof with DO.
(time (repeat 1000 (do ((i 0 (+ 1 i))
			(l (%make-queue 0) (%enqueue! l i)))
		       ((= i 1000)
			(%queue-list-ref l)))))

(time (repeat 1000 (do ((i 0 (+ 1 i))
			(l '(0) (cons i l)))
		       ((= i 1000)
			(reverse l)))))

;;The proof with LET.
(time (repeat 1000 (let loop ((l (%make-queue 0))
			      (i 0))
		     (if (= i 1000)
			 (%queue-list-ref l)
		       (begin
			 (%enqueue! l i)
			 (loop l (+ 1 i)))))))

(time (repeat 1000 (let loop ((l '(0))
			      (i 0))
		     (if (= i 1000)
			 (reverse l)
		       (loop (cons i l) (+ 1 i))))))

;;; end of file

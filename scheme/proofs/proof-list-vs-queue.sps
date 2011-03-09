;;;Part of: Nausicaa/Scheme
;;;Contents: proofs about lists
;;;Date: Tue Jul 28, 2009
;;;
;;;Abstract
;;;
;;;	Time building a list with  the queue functions from (lists low),
;;;	versus building  it in reverse  than calling REVERSE.   It seems
;;;	that  the queue  is faster  with  Ikarus, slower  with Mosh  and
;;;	Ypsilon.
;;;
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


#!r6rs
(import (rnrs)
  (nausicaa profiling)
  (rnrs mutable-pairs))

(define %queue-list-ref		car)
(define %queue-last-pair-ref	cdr)
(define %queue-last-pair-set!	set-cdr!)

(define-syntax %make-queue
  (syntax-rules ()
    ((_ ?obj)
     (let ((pair (cons ?obj '())))
       (cons pair pair)))))

(define-syntax %enqueue!
  (syntax-rules ()
    ((_ ?queue ?obj)
     (begin
       (set-cdr! (cdr ?queue) (cons ?obj '()))
       (cons (car ?queue) (cddr ?queue))))))


;;;; warm up

(write (do ((i 1 (+ 1 i))
	    (l (%make-queue 0) (%enqueue! l i)))
	   ((= i 10)
	    (%queue-list-ref l))))
(newline)

(write (do ((i 1 (+ 1 i))
	    (l '(0) (cons i l)))
	   ((= i 10)
	    (reverse l))))
(newline)

(write (do ((i 1 (+ 1 i))
	    (l '(0) (cons i l)))
	   ((= i 10)
	    (reverse l))))
(newline)

(write (let loop ((l '(0))
		  (i 1))
	 (if (= i 10)
	     (reverse l)
	   (loop (cons i l) (+ 1 i)))))
(newline)
(newline)


(define warmup-times	1000)
(define repeat-times	1000)
(define max-value+1	10000)

(repeat warmup-times (do ((i 1 (+ 1 i))
			  (l (%make-queue 0) (%enqueue! l i)))
			 ((= i max-value+1)
			  (%queue-list-ref l))))

(repeat warmup-times (do ((i 1 (+ 1 i))
			  (l '(0) (cons i l)))
			 ((= i max-value+1)
			  (reverse l))))

;;The proof with DO.
(time (repeat repeat-times (do ((i 1 (+ 1 i))
				(l (%make-queue 0) (%enqueue! l i)))
			       ((= i max-value+1)
				(%queue-list-ref l)))))
(newline)

(time (repeat repeat-times (do ((i 1 (+ 1 i))
				(l '(0) (cons i l)))
			       ((= i max-value+1)
				(reverse l)))))
(newline)
(newline)
(newline)

;;The proof with LET.
(time (repeat repeat-times (let loop ((l (%make-queue 0))
				      (i 1))
			     (if (= i max-value+1)
				 (%queue-list-ref l)
			       (loop (%enqueue! l i) (+ 1 i))))))
(newline)

(time (repeat repeat-times (let loop ((l '(0))
				      (i 1))
			     (if (= i max-value+1)
				 (reverse l)
			       (loop (cons i l) (+ 1 i))))))
(newline)

;;; end of file

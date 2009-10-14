;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: queue record definition
;;;Date: Wed Oct 14, 2009
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


(library (queues)
  (export
    <queue>		<queue-rtd>
    <queue*>		queue

    queue-empty?	queue-length

    queue-front		queue-rear
    queue-push!		queue-pop!
    queue-enqueue!	(rename (queue-pop! queue-dequeue!))

    queue-find		queue-for-all
    queue-exists

    queue-remp!		queue-remove!
    queue-remv!		queue-remq!
    queue-filter!

    queue-memp		queue-member
    queue-memv		queue-memq

    queue->list		list->queue
    queue->vector	vector->queue)
  (import (rnrs)
    (records)
    (rnrs mutable-pairs)
    (for (queues types) expand run)
    (for (queues extensions) expand run))


;;; helpers

(define-syntax last-pair/stx
  (syntax-rules ()
    ((_ ?x)
     (let ((x ?x))
       (if (null? x)
	   #f
	 (let loop ((x ?x))
	   (if (pair? (cdr x))
	       (loop (cdr x))
	     x)))))))

(define-syntax list-copy/stx
  (syntax-rules ()
    ((_ ?ell)
     (let loop ((ell ?ell))
       (if (pair? ell)
	   (cons (car ell) (loop (cdr ell)))
	 ell)))))


(define queue
  (case-lambda
   (()
    (make-<queue> '() #f))
   (args
    (make-<queue> args (last-pair/stx args)))))


(define (queue-push! value que)
  ;;Push VALUE at the beginning of QUE.
  ;;
  (assert (<queue>? que))
  (with-fields (((first-pair last-pair) <queue-rtd> que))
    (let ((first (cons value first-pair)))
      (set! first-pair first)
      (or last-pair (set! last-pair first)))))

(define (queue-pop! que)
  ;;Pop a value from the beginning of QUE.
  ;;
  (assert (<queue>? que))
  (with-fields (((first-pair last-pair) <queue-rtd> que))
    (let ((first first-pair))
      (if (null? first)
	  (error 'queue-pop! "queue is empty" que)
	(begin
	  (set! first-pair (cdr first))
	  (when (eq? last-pair first)
	    (set! last-pair #f))))
      (car first))))

(define (queue-enqueue! que obj)
  ;;Push VALUE at the end of QUE.
  ;;
  (assert (<queue>? que))
  (with-fields (((first-pair last-pair) <queue-rtd> que))
    (let ((last (list obj)))
      (if (null? first-pair)
	  (set! first-pair last)
	(set-cdr! last-pair last))
      (set! last-pair last))))


(define (queue-find proc que)
  (assert (<queue>? que))
  (assert (procedure? proc))
  (find proc (<queue>-first-pair que)))

(define (queue-for-all proc que)
  (assert (<queue>? que))
  (assert (procedure? proc))
  (for-all proc (<queue>-first-pair que)))

(define (queue-exists proc que)
  (assert (<queue>? que))
  (assert (procedure? proc))
  (exists proc (<queue>-first-pair que)))


(define (%remove remover thing que)
  (with-fields (((first-pair last-pair) <queue-rtd> que))
    (set! first-pair (remover thing first-pair))
    (set! last-pair (last-pair/stx first-pair))))

(define (queue-remp! proc que)
  (assert (<queue>? que))
  (assert (procedure? proc))
  (%remove remp proc que))

(define (queue-remove! obj que)
  (assert (<queue>? que))
  (%remove remove obj que))

(define (queue-remv! obj que)
  (assert (<queue>? que))
  (%remove remv obj que))

(define (queue-remq! obj que)
  (assert (<queue>? que))
  (%remove remq obj que))

(define (queue-filter! proc que)
  (assert (<queue>? que))
  (assert (procedure? proc))
  (%remove filter proc que))


(define (queue-memp proc que)
  (assert (<queue>? que))
  (assert (procedure? proc))
  (memp proc (<queue>-first-pair que)))

(define (queue-member obj que)
  (assert (<queue>? que))
  (member obj (<queue>-first-pair que)))

(define (queue-memv obj que)
  (assert (<queue>? que))
  (memv obj (<queue>-first-pair que)))

(define (queue-memq obj que)
  (assert (<queue>? que))
  (memq obj (<queue>-first-pair que)))


(define (queue->list que)
  (assert (<queue>? que))
  (list-copy/stx (<queue>-first-pair que)))

(define (list->queue ell)
  (assert (list? ell))
  (let ((ell (list-copy/stx ell)))
    (make-<queue> ell (last-pair/stx ell))))

(define (queue->vector que)
  (assert (<queue>? que))
  (list->vector (<queue>-first-pair que)))

(define (vector->queue vec)
  (assert (vector? vec))
  (let ((ell (vector->list vec)))
    (make-<queue> ell (last-pair/stx ell))))


;;;; done

)

;;; end of file

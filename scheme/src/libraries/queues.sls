;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: queue record definition
;;;Date: Wed Oct 14, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
    <queue>		<queue>?	<queue>-with-record-fields-of
    make-<queue>
    <queue>-first-pair	<queue>-first-pair-set!
    <queue>-last-pair	<queue>-last-pair-set!

    queue-empty?	queue-length

    queue-front		queue-rear
    queue-push!		queue-pop!
    queue-enqueue!	(rename (queue-pop! queue-dequeue!))
    queue-purge!

    queue-find		queue-for-all
    queue-exists

    queue-remp!		queue-remove!
    queue-remv!		queue-remq!
    queue-filter!

    queue-memp		queue-member
    queue-memv		queue-memq

    queue->list		list->queue
    queue->vector	vector->queue)
  (import (nausicaa)
    (rnrs mutable-pairs))


;;; helpers

(define-inline (last-pair/stx ?x)
  ;;*WARNING* Do  not rename LAST-PAIR/STX to LAST-PAIR,  it would clash
  ;;with the LAST-PAIR field of <queue> records.
  ;;
  (let ((x ?x))
    (if (null? x)
	#f
      (let loop ((x x))
	(if (pair? (cdr x))
	    (loop (cdr x))
	  x)))))

(define-syntax list-copy
  (syntax-rules ()
    ((_ ?ell)
     (let loop ((ell ?ell))
       (if (pair? ell)
	   (cons (car ell) (loop (cdr ell)))
	 ell)))))


(define-class <queue>
  (nongenerative nausicaa:queues:<queue>)
  (protocol (lambda (make-<top>)
	      (case-lambda
	       (()
		((make-<top>) '() #f))
	       (args
		((make-<top>) args (last-pair/stx args))))))
  (fields (mutable first-pair)
	  (mutable last-pair))
  (virtual-fields (immutable front	queue-front)
		  (immutable rear	queue-rear)
		  (immutable empty?	queue-empty?)
		  (immutable length	queue-length)))


(define (queue-front (que <queue>))
  (if (null? que.first-pair)
      (error 'queue-front "queue is empty" que)
    (car que.first-pair)))

(define (queue-rear (que <queue>))
  (if que.last-pair
      (car que.last-pair)
    (error 'queue-rear "queue is empty" que)))

(define (queue-empty? (que <queue>))
  (null? que.first-pair))

(define (queue-length (que <queue>))
  (length que.first-pair))


(define (queue-push! value (que <queue>))
  ;;Push VALUE at the beginning of QUE.
  ;;
  (let ((first (cons value que.first-pair)))
    (set! que.first-pair first)
    (or que.last-pair (set! que.last-pair first))))

(define (queue-pop! (que <queue>))
  ;;Pop a value from the beginning of QUE.
  ;;
  (let ((first que.first-pair))
    (if (null? first)
	(error 'queue-pop! "queue is empty" que)
      (begin
	(set! que.first-pair (cdr first))
	(when (eq? que.last-pair first)
	  (set! que.last-pair #f))))
    (car first)))

(define (queue-enqueue! (que <queue>) obj)
  ;;Push VALUE at the end of QUE.
  ;;
  (let ((last (list obj)))
    (if (null? que.first-pair)
	(set! que.first-pair last)
      (set-cdr! que.last-pair last))
    (set! que.last-pair last)))

(define (queue-purge! (que <queue>))
  (set! que.first-pair '())
  (set! que.last-pair  #f))


(define (queue-find proc (que <queue>))
  (find proc que.first-pair))

(define (queue-for-all proc (que <queue>))
  (for-all proc que.first-pair))

(define (queue-exists proc (que <queue>))
  (exists proc que.first-pair))


(define (%remove remover thing (que <queue>))
  (set! que.first-pair (remover thing que.first-pair))
  (set! que.last-pair (last-pair/stx que.first-pair)))

(define (queue-remp! proc que)
  (%remove remp proc que))

(define (queue-remove! obj que)
  (%remove remove obj que))

(define (queue-remv! obj que)
  (%remove remv obj que))

(define (queue-remq! obj que)
  (%remove remq obj que))

(define (queue-filter! proc que)
  (%remove filter proc que))


(define (queue-memp proc (que <queue>))
  (memp proc que.first-pair))

(define (queue-member obj (que <queue>))
  (member obj que.first-pair))

(define (queue-memv obj (que <queue>))
  (memv obj que.first-pair))

(define (queue-memq obj (que <queue>))
  (memq obj que.first-pair))


(define (queue->list (que <queue>))
  (list-copy que.first-pair))

(define (list->queue ell)
  (apply make-<queue> (list-copy ell)))

(define (queue->vector (que <queue>))
  (list->vector que.first-pair))

(define (vector->queue vec)
  (apply make-<queue> (vector->list vec)))


;;;; done

)

;;; end of file

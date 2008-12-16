;;; q.scm --- Queues
;;;
;;; Assimilated into Nausicaa for Ikarus Scheme: Fri Nov  7, 2008.
;;;
;;; Copyright (C) 2008 Marco Maggi <marcomaggi@gna.org>
;;; Copyright (C) 1995, 2001, 2004, 2006 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or modify
;;; it  under the  terms of  the GNU  Lesser General  Public  License as
;;; published by the Free Software Foundation; either version 2.1 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE.  See  the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should  have received  a copy of  the GNU Lesser  General Public
;;; License along with this library;  if not, write to the Free Software
;;; Foundation,  Inc.,  51  Franklin  Street, Fifth  Floor,  Boston,  MA
;;; 02110-1301 USA
;;;

(library (uriel queue)
  (export
    sync-q! make-q q? q-empty? q-empty-check q-front q-rear
    q-remove! q-push! enq! q-pop! deq! q-length)
  (import (r6rs)
    (rnrs mutable-pairs (6))
    (list-lib))

  (define (sync-q! q)
    (set-cdr! q (if (pair? (car q)) (last-pair (car q))
		  #f))
    q)

  (define (make-q) (cons '() #f))

  (define (q? obj)
    (and (pair? obj)
	 (if (pair? (car obj))
	     (eq? (cdr obj) (last-pair (car obj)))
	   (and (null? (car obj))
		(not (cdr obj))))))

  (define (q-empty? obj) (null? (car obj)))

  (define (q-empty-check q) (if (q-empty? q) (raise 'queue-is-empty)))

  (define (q-front q) (q-empty-check q) (caar q))

  (define (q-rear q) (q-empty-check q) (cadr q))

  (define (q-remove! q obj)
    (set-car! q (remove! (lambda (o)
			   (eq? o obj)) (car q)))
    (sync-q! q))

  (define (q-push! q obj)
    (let ((h (cons obj (car q))))
      (set-car! q h)
      (or (cdr q) (set-cdr! q h)))
    q)

  (define (enq! q obj)
    (let ((h (cons obj '())))
      (if (null? (car q))
	  (set-car! q h)
	(set-cdr! (cdr q) h))
      (set-cdr! q h))
    q)

  (define (q-pop! q)
    (q-empty-check q)
    (let ((it (caar q))
	  (next (cdar q)))
      (if (null? next)
	  (set-cdr! q #f))
      (set-car! q next)
      it))

  (define deq! q-pop!)

  (define (q-length q) (length (car q))))

;;; end of file

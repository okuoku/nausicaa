;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record extension for queues
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


(library (queues extensions)
  (export
    <queue*>
    queue-front queue-rear
    queue-empty? queue-length)
  (import (rnrs)
    (records)
    (queues types))

  (define (queue-front que)
    (assert (<queue>? que))
    (let ((p (<queue>-first-pair que)))
      (if (null? p)
	  (error 'queue-front "queue is empty" que)
	(car p))))

  (define (queue-rear que)
    (assert (<queue>? que))
    (let ((p (<queue>-last-pair que)))
      (if p
	  (car p)
	(error 'queue-rear "queue is empty" que))))

  (define (queue-empty? que)
    (assert (<queue>? que))
    (null? (<queue>-first-pair que)))

  (define (queue-length que)
    (assert (<queue>? que))
    (length (<queue>-first-pair que)))

  (define-record-extension <queue*>
    (parent <queue>)
    (fields (front	queue-front	#f)
	    (rear	queue-rear	#f)
	    (empty?	queue-empty?	#f)
	    (length	queue-length	#f))))

;;; end of file

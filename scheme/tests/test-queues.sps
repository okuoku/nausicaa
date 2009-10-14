;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for queues
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


(import (nausicaa)
  (checks)
  (records)
  (for (queues) expand run))

(check-set-mode! 'report-failed)
(display "*** testing queues\n")


(parametrise ((check-test-name 'making))

  (check
      (let ((q (queue)))
	(is-a? q <queue>))
    => #t)

  (check
      (let ((q (queue 1)))
	(is-a? q <queue>))
    => #t)

  (check
      (let ((q (queue 1 2 3)))
	(is-a? q <queue>))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (queue->list (queue))
    => '())

  (check
      (queue->list (queue 1))
    => '(1))

  (check
      (queue->list (queue 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name 'pred))

  (check
      (queue-empty? (queue))
    => #t)

  (check
      (queue-empty? (queue 1))
    => #f)

  (check
      (queue-empty? (queue 1 2 3))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-fields ((empty? <queue*> (queue)))
	empty?)
    => #t)

  (check
      (with-fields ((empty? <queue*> (queue 1)))
	empty?)
    => #f)

  (check
      (with-fields ((empty? <queue*> (queue 1 2 3)))
	empty?)
    => #f)

  #t)


(parametrise ((check-test-name 'inspect))

  (check
      (queue-length (queue))
    => 0)

  (check
      (queue-length (queue 1))
    => 1)

  (check
      (queue-length (queue 1 2 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (with-fields ((length <queue*> (queue)))
	length)
    => 0)

  (check
      (with-fields ((length <queue*> (queue 1)))
	length)
    => 1)

  (check
      (with-fields ((length <queue*> (queue 1 2 3)))
	length)
    => 3)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(queue-front (queue)))
    => "queue is empty")

  (check
      (queue-front (queue 1))
    => 1)

  (check
      (queue-front (queue 1 2 3))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(with-fields ((front <queue*> (queue)))
	  front))
    => "queue is empty")

  (check
      (with-fields ((front <queue*> (queue 1)))
	front)
    => 1)

  (check
      (with-fields ((front <queue*> (queue 1 2 3)))
	front)
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(queue-rear (queue)))
    => "queue is empty")

  (check
      (queue-rear (queue 1))
    => 1)

  (check
      (queue-rear (queue 1 2 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(with-fields ((rear <queue*> (queue)))
	  rear))
    => "queue is empty")

  (check
      (with-fields ((rear <queue*> (queue 1)))
	rear)
    => 1)

  (check
      (with-fields ((rear <queue*> (queue 1 2 3)))
	rear)
    => 3)

  #t)


(parametrise ((check-test-name 'operations))

  (check
      (let ((q (queue)))
	(queue-enqueue! q 1)
	(queue-enqueue! q 2)
	(queue-enqueue! q 3)
	(queue->list q))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-push! 1 q)
	(queue-push! 2 q)
	(queue-push! 3 q)
	(queue->list q))
    => '(3 2 1))

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (queue)))
	  (queue-dequeue! q)))
    => "queue is empty")

  (check
      (let ((q (queue 1 2 3)))
	(queue-dequeue! q))
    => 1)

  (check
      (let ((q (queue 1 2 3)))
	(queue-dequeue! q)
	(queue-dequeue! q)
	(queue-dequeue! q))
    => 3)

  (check
      (let ((q (queue 1 2 3)))
	(queue-dequeue! q)
	(queue-dequeue! q)
	(queue-dequeue! q)
	(queue-empty? q))
    => #t)

  #t)


(parametrise ((check-test-name 'list))

  (check
      (let ((q (queue)))
	(queue-find even? q))
    => #f)

  (check
      (let ((q (queue 1 2)))
	(queue-find even? q))
    => 2)

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-find even? q))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-exists even? q))
    => #f)

  (check
      (let ((q (queue 1 2)))
	(queue-exists even? q))
    => #t)

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-exists even? q))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-for-all even? q))
    => #t)

  (check
      (let ((q (queue 1 2)))
	(queue-for-all even? q))
    => #f)

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-for-all even? q))
    => #f)

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-for-all even? q))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-remp! even? q)
	(queue->list q))
    => '())

  (check
      (let ((q (queue 1 2)))
	(queue-remp! even? q)
	(queue->list q))
    => '(1))

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-remp! even? q)
	(queue->list q))
    => '(1 3))

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-remp! even? q)
	(queue->list q))
    => '())

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-remove! 2 q)
	(queue->list q))
    => '())

  (check
      (let ((q (queue 1 2)))
	(queue-remove! 2 q)
	(queue->list q))
    => '(1))

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-remove! 2 q)
	(queue->list q))
    => '(1 3 4))

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-remove! 2 q)
	(queue->list q))
    => '(4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-remv! 2 q)
	(queue->list q))
    => '())

  (check
      (let ((q (queue 1 2)))
	(queue-remv! 2 q)
	(queue->list q))
    => '(1))

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-remv! 2 q)
	(queue->list q))
    => '(1 3 4))

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-remv! 2 q)
	(queue->list q))
    => '(4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-remq! 'two q)
	(queue->list q))
    => '())

  (check
      (let ((q (queue 1 'two)))
	(queue-remq! 'two q)
	(queue->list q))
    => '(1))

  (check
      (let ((q (queue 1 'two 3 4)))
	(queue-remq! 'two q)
	(queue->list q))
    => '(1 3 4))

  (check
      (let ((q (queue 'two 4 6 8)))
	(queue-remq! 'two q)
	(queue->list q))
    => '(4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-memp even? q))
    => #f)

  (check
      (let ((q (queue 1 2)))
	(queue-memp even? q))
    => '(2))

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-memp even? q))
    => '(2 3 4))

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-memp even? q))
    => '(2 4 6 8))

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-memp odd? q))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-member 2 q))
    => #f)

  (check
      (let ((q (queue 1 2)))
	(queue-member 2 q))
    => '(2))

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-member 2 q))
    => '(2 3 4))

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-member 2 q))
    => '(2 4 6 8))

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-member 10 q))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-memv 2 q))
    => #f)

  (check
      (let ((q (queue 1 2)))
	(queue-memv 2 q))
    => '(2))

  (check
      (let ((q (queue 1 2 3 4)))
	(queue-memv 2 q))
    => '(2 3 4))

  (check
      (let ((q (queue 2 4 6 8)))
	(queue-memv 2 q))
    => '(2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (queue)))
	(queue-memq 'two q))
    => #f)

  (check
      (let ((q (queue 1 'two)))
	(queue-memq 'two q))
    => '(two))

  (check
      (let ((q (queue 1 'two 3 4)))
	(queue-memq 'two q))
    => '(two 3 4))

  (check
      (let ((q (queue 'two 4 6 8)))
	(queue-memq 'two q))
    => '(two 4 6 8))

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name 'conversion))

  (check
      (queue->list (queue))
    => '())

  (check
      (queue->list (queue 1))
    => '(1))

  (check
      (queue->list (queue 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (queue->vector (queue))
    => '#())

  (check
      (queue->vector (queue 1))
    => '#(1))

  (check
      (queue->vector (queue 1 2 3))
    => '#(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (queue->list (list->queue '()))
    => '())

  (check
      (queue->list (list->queue '(1)))
    => '(1))

  (check
      (queue->list (list->queue '(1 2 3)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (queue->vector (vector->queue '#()))
    => '#())

  (check
      (queue->vector (vector->queue '#(1)))
    => '#(1))

  (check
      (queue->vector (vector->queue '#(1 2 3)))
    => '#(1 2 3))

  #t)



(check-report)

;;; end of file

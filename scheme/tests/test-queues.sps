;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for queues
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


(import (nausicaa)
  (checks)
  (queues))

(check-set-mode! 'report-failed)
(display "*** testing queues\n")


(parametrise ((check-test-name 'making))

  (check
      (let ((q (make-<queue>)))
	(is-a? q <queue>))
    => #t)

  (check
      (let ((q (make-<queue> 1)))
	(is-a? q <queue>))
    => #t)

  (check
      (let ((q (make-<queue> 1 2 3)))
	(is-a? q <queue>))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((q (make* <queue>)))
	(is-a? q <queue>))
    => #t)

  (check
      (let ((q (make* <queue> 1)))
	(is-a? q <queue>))
    => #t)

  (check
      (let ((q (make* <queue> 1 2)))
	(is-a? q <queue>))
    => #t)

  (check
      (let ((q (make* <queue> 1 2 3)))
	(is-a? q <queue>))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (queue->list (make-<queue>))
    => '())

  (check
      (queue->list (make-<queue> 1))
    => '(1))

  (check
      (queue->list (make-<queue> 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name 'pred))

  (check
      (queue-empty? (make-<queue>))
    => #t)

  (check
      (queue-empty? (make-<queue> 1))
    => #f)

  (check
      (queue-empty? (make-<queue> 1 2 3))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let (((q <queue>) (make-<queue>)))
  	q.empty?)
    => #t)

  (check
      (let (((q <queue>) (make-<queue> 1)))
  	q.empty?)
    => #f)

  (check
      (let (((q <queue>) (make-<queue> 1 2 3)))
  	q.empty?)
    => #f)

  #t)


(parametrise ((check-test-name 'inspect))

  (check
      (queue-length (make-<queue>))
    => 0)

  (check
      (queue-length (make-<queue> 1))
    => 1)

  (check
      (queue-length (make-<queue> 1 2 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (let (((q <queue>) (make-<queue>)))
	q.length)
    => 0)

  (check
      (let (((q <queue>) (make-<queue> 1)))
	q.length)
    => 1)

  (check
      (let (((q <queue>) (make-<queue> 1 2 3)))
	q.length)
    => 3)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(queue-front (make-<queue>)))
    => "queue is empty")

  (check
      (queue-front (make-<queue> 1))
    => 1)

  (check
      (queue-front (make-<queue> 1 2 3))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let (((q <queue>) (make-<queue>)))
	  (q.front)))
    => "queue is empty")

  (check
      (let (((q <queue>) (make-<queue> 1)))
	(q.front))
    => 1)

  (check
      (let (((q <queue>) (make-<queue> 1 2 3)))
	(q.front))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(queue-rear (make-<queue>)))
    => "queue is empty")

  (check
      (queue-rear (make-<queue> 1))
    => 1)

  (check
      (queue-rear (make-<queue> 1 2 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let (((q <queue>) (make-<queue>)))
	  (q.rear)))
    => "queue is empty")

  (check
      (let (((q <queue>) (make-<queue> 1)))
	(q.rear))
    => 1)

  (check
      (let (((q <queue>) (make-<queue> 1 2 3)))
	(q.rear))
    => 3)

  #t)


(parametrise ((check-test-name 'operations))

  (check
      (let ((q (make-<queue> 1 2 3)))
	(queue-purge! q)
	(queue-empty? q))
    => #t)

  (check
      (let (((q <queue>) (make-<queue> 1 2 3)))
	(q.purge!)
	q.empty?)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-enqueue! q 1)
	(queue-enqueue! q 2)
	(queue-enqueue! q 3)
	(queue->list q))
    => '(1 2 3))

  (check
      (let (((q <queue>) (make-<queue>)))
	(q.enqueue! 1)
	(q.enqueue! 2)
	(q.enqueue! 3)
	(q.list))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-push! 1 q)
	(queue-push! 2 q)
	(queue-push! 3 q)
	(queue->list q))
    => '(3 2 1))

  (check
      (let (((q <queue>) (make-<queue>)))
	(q.push! 1)
	(q.push! 2)
	(q.push! 3)
	(q.list))
    => '(3 2 1))

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (make-<queue>)))
	  (queue-dequeue! q)))
    => "queue is empty")

  (check
      (let ((q (make-<queue> 1 2 3)))
	(queue-dequeue! q))
    => 1)

  (check
      (let ((q (make-<queue> 1 2 3)))
	(queue-dequeue! q)
	(queue-dequeue! q)
	(queue-dequeue! q))
    => 3)

  (check
      (let ((q (make-<queue> 1 2 3)))
	(queue-dequeue! q)
	(queue-dequeue! q)
	(queue-dequeue! q)
	(queue-empty? q))
    => #t)

  (check
      (let (((q <queue>) (make-<queue> 1 2 3)))
	(q.dequeue!)
	(q.dequeue!)
	(q.dequeue!))
    => 3)
  #t)


(parametrise ((check-test-name 'list))

  (check
      (let ((q (make-<queue>)))
	(queue-find even? q))
    => #f)

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-find even? q))
    => 2)

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-find even? q))
    => 2)

  (check
      (let (((q <queue>) (make-<queue> 1 2)))
	(q.find even?))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-exists even? q))
    => #f)

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-exists even? q))
    => #t)

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-exists even? q))
    => #t)

  (check
      (let (((q <queue>) (make-<queue> 1 2)))
	(q.exists even?))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-for-all even? q))
    => #t)

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-for-all even? q))
    => #f)

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-for-all even? q))
    => #f)

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-for-all even? q))
    => #t)

  (check
      (let (((q <queue>) (make-<queue> 1 2 3 4)))
	(q.for-all even?))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-remp! even? q)
	(queue->list q))
    => '())

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-remp! even? q)
	(queue->list q))
    => '(1))

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-remp! even? q)
	(queue->list q))
    => '(1 3))

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-remp! even? q)
	(queue->list q))
    => '())

  (check
      (let (((q <queue>) (make-<queue> 1 2 3 4)))
	(q.remp! even?)
	(q.list))
    => '(1 3))

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-remove! 2 q)
	(queue->list q))
    => '())

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-remove! 2 q)
	(queue->list q))
    => '(1))

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-remove! 2 q)
	(queue->list q))
    => '(1 3 4))

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-remove! 2 q)
	(queue->list q))
    => '(4 6 8))

  (check
      (let (((q <queue>) (make-<queue> 1 2 3 4)))
	(q.remove! 2)
	(q.list))
    => '(1 3 4))

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-remv! 2 q)
	(queue->list q))
    => '())

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-remv! 2 q)
	(queue->list q))
    => '(1))

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-remv! 2 q)
	(queue->list q))
    => '(1 3 4))

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-remv! 2 q)
	(queue->list q))
    => '(4 6 8))

  (check
      (let (((q <queue>) (make-<queue> 1 2 3 4)))
	(q.remv! 2)
	(q.list))
    => '(1 3 4))

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-remq! 'two q)
	(queue->list q))
    => '())

  (check
      (let ((q (make-<queue> 1 'two)))
	(queue-remq! 'two q)
	(queue->list q))
    => '(1))

  (check
      (let ((q (make-<queue> 1 'two 3 4)))
	(queue-remq! 'two q)
	(queue->list q))
    => '(1 3 4))

  (check
      (let ((q (make-<queue> 'two 4 6 8)))
	(queue-remq! 'two q)
	(queue->list q))
    => '(4 6 8))

  (check
      (let (((q <queue>) (make-<queue> 1 'two 3 4)))
	(q.remq! 'two)
	(q.list))
    => '(1 3 4))

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-memp even? q))
    => #f)

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-memp even? q))
    => '(2))

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-memp even? q))
    => '(2 3 4))

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-memp even? q))
    => '(2 4 6 8))

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-memp odd? q))
    => #f)

  (check
      (let (((q <queue>) (make-<queue> 2 4 6 8)))
	(q.memp even?))
    => '(2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-member 2 q))
    => #f)

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-member 2 q))
    => '(2))

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-member 2 q))
    => '(2 3 4))

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-member 2 q))
    => '(2 4 6 8))

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-member 10 q))
    => #f)

  (check
      (let (((q <queue>) (make-<queue> 1 2 3 4)))
	(q.member 2))
    => '(2 3 4))

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-memv 2 q))
    => #f)

  (check
      (let ((q (make-<queue> 1 2)))
	(queue-memv 2 q))
    => '(2))

  (check
      (let ((q (make-<queue> 1 2 3 4)))
	(queue-memv 2 q))
    => '(2 3 4))

  (check
      (let ((q (make-<queue> 2 4 6 8)))
	(queue-memv 2 q))
    => '(2 4 6 8))

  (check
      (let (((q <queue>) (make-<queue> 1 2 3 4)))
	(q.memv 2))
    => '(2 3 4))

;;; --------------------------------------------------------------------

  (check
      (let ((q (make-<queue>)))
	(queue-memq 'two q))
    => #f)

  (check
      (let ((q (make-<queue> 1 'two)))
	(queue-memq 'two q))
    => '(two))

  (check
      (let ((q (make-<queue> 1 'two 3 4)))
	(queue-memq 'two q))
    => '(two 3 4))

  (check
      (let ((q (make-<queue> 'two 4 6 8)))
	(queue-memq 'two q))
    => '(two 4 6 8))

  (check
      (let (((q <queue>) (make-<queue> 1 'two 3 4)))
	(q.memq 'two))
    => '(two 3 4))

  #t)


(parametrise ((check-test-name 'conversion))

  (check
      (queue->list (make-<queue>))
    => '())

  (check
      (queue->list (make-<queue> 1))
    => '(1))

  (check
      (queue->list (make-<queue> 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (queue->vector (make-<queue>))
    => '#())

  (check
      (queue->vector (make-<queue> 1))
    => '#(1))

  (check
      (queue->vector (make-<queue> 1 2 3))
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


;;;; done

(check-report)

;;; end of file

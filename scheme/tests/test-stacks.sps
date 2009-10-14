;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for stacks
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
  (for (stacks) expand run))

(check-set-mode! 'report-failed)
(display "*** testing stacks\n")


(parametrise ((check-test-name 'making))

  (check
      (let ((q (stack)))
	(is-a? q <stack>))
    => #t)

  (check
      (let ((q (stack 1)))
	(is-a? q <stack>))
    => #t)

  (check
      (let ((q (stack 1 2 3)))
	(is-a? q <stack>))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (stack->list (stack))
    => '())

  (check
      (stack->list (stack 1))
    => '(1))

  (check
      (stack->list (stack 1 2 3))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name 'pred))

  (check
      (stack-empty? (stack))
    => #t)

  (check
      (stack-empty? (stack 1))
    => #f)

  (check
      (stack-empty? (stack 1 2 3))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (with-fields ((empty? <stack*> (stack)))
	empty?)
    => #t)

  (check
      (with-fields ((empty? <stack*> (stack 1)))
	empty?)
    => #f)

  (check
      (with-fields ((empty? <stack*> (stack 1 2 3)))
	empty?)
    => #f)

  #t)


(parametrise ((check-test-name 'inspect))

  (check
      (stack-length (stack))
    => 0)

  (check
      (stack-length (stack 1))
    => 1)

  (check
      (stack-length (stack 1 2 3))
    => 3)

;;; --------------------------------------------------------------------

  (check
      (with-fields ((length <stack*> (stack)))
	length)
    => 0)

  (check
      (with-fields ((length <stack*> (stack 1)))
	length)
    => 1)

  (check
      (with-fields ((length <stack*> (stack 1 2 3)))
	length)
    => 3)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(stack-top (stack)))
    => "stack is empty")

  (check
      (stack-top (stack 1))
    => 1)

  (check
      (stack-top (stack 1 2 3))
    => 1)

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(with-fields ((top <stack*> (stack)))
	  top))
    => "stack is empty")

  (check
      (with-fields ((top <stack*> (stack 1)))
	top)
    => 1)

  (check
      (with-fields ((top <stack*> (stack 1 2 3)))
	top)
    => 1)

  #t)


(parametrise ((check-test-name 'operations))

  (check
      (let ((q (stack)))
	(stack-push! 1 q)
	(stack-push! 2 q)
	(stack-push! 3 q)
	(stack->list q))
    => '(3 2 1))

;;; --------------------------------------------------------------------

  (check
      (guard (E (else (condition-message E)))
	(let ((q (stack)))
	  (stack-pop! q)))
    => "stack is empty")

  (check
      (let ((q (stack 1 2 3)))
	(stack-pop! q))
    => 1)

  (check
      (let ((q (stack 1 2 3)))
	(stack-pop! q)
	(stack-pop! q)
	(stack-pop! q))
    => 3)

  (check
      (let ((q (stack 1 2 3)))
	(stack-pop! q)
	(stack-pop! q)
	(stack-pop! q)
	(stack-empty? q))
    => #t)

  #t)


(parametrise ((check-test-name 'list))

  (check
      (let ((q (stack)))
	(stack-find even? q))
    => #f)

  (check
      (let ((q (stack 1 2)))
	(stack-find even? q))
    => 2)

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-find even? q))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-exists even? q))
    => #f)

  (check
      (let ((q (stack 1 2)))
	(stack-exists even? q))
    => #t)

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-exists even? q))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-for-all even? q))
    => #t)

  (check
      (let ((q (stack 1 2)))
	(stack-for-all even? q))
    => #f)

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-for-all even? q))
    => #f)

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-for-all even? q))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-remp! even? q)
	(stack->list q))
    => '())

  (check
      (let ((q (stack 1 2)))
	(stack-remp! even? q)
	(stack->list q))
    => '(1))

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-remp! even? q)
	(stack->list q))
    => '(1 3))

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-remp! even? q)
	(stack->list q))
    => '())

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-remove! 2 q)
	(stack->list q))
    => '())

  (check
      (let ((q (stack 1 2)))
	(stack-remove! 2 q)
	(stack->list q))
    => '(1))

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-remove! 2 q)
	(stack->list q))
    => '(1 3 4))

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-remove! 2 q)
	(stack->list q))
    => '(4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-remv! 2 q)
	(stack->list q))
    => '())

  (check
      (let ((q (stack 1 2)))
	(stack-remv! 2 q)
	(stack->list q))
    => '(1))

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-remv! 2 q)
	(stack->list q))
    => '(1 3 4))

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-remv! 2 q)
	(stack->list q))
    => '(4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-remq! 'two q)
	(stack->list q))
    => '())

  (check
      (let ((q (stack 1 'two)))
	(stack-remq! 'two q)
	(stack->list q))
    => '(1))

  (check
      (let ((q (stack 1 'two 3 4)))
	(stack-remq! 'two q)
	(stack->list q))
    => '(1 3 4))

  (check
      (let ((q (stack 'two 4 6 8)))
	(stack-remq! 'two q)
	(stack->list q))
    => '(4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-memp even? q))
    => #f)

  (check
      (let ((q (stack 1 2)))
	(stack-memp even? q))
    => '(2))

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-memp even? q))
    => '(2 3 4))

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-memp even? q))
    => '(2 4 6 8))

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-memp odd? q))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-member 2 q))
    => #f)

  (check
      (let ((q (stack 1 2)))
	(stack-member 2 q))
    => '(2))

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-member 2 q))
    => '(2 3 4))

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-member 2 q))
    => '(2 4 6 8))

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-member 10 q))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-memv 2 q))
    => #f)

  (check
      (let ((q (stack 1 2)))
	(stack-memv 2 q))
    => '(2))

  (check
      (let ((q (stack 1 2 3 4)))
	(stack-memv 2 q))
    => '(2 3 4))

  (check
      (let ((q (stack 2 4 6 8)))
	(stack-memv 2 q))
    => '(2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (let ((q (stack)))
	(stack-memq 'two q))
    => #f)

  (check
      (let ((q (stack 1 'two)))
	(stack-memq 'two q))
    => '(two))

  (check
      (let ((q (stack 1 'two 3 4)))
	(stack-memq 'two q))
    => '(two 3 4))

  (check
      (let ((q (stack 'two 4 6 8)))
	(stack-memq 'two q))
    => '(two 4 6 8))

;;; --------------------------------------------------------------------

  #t)


(parametrise ((check-test-name 'conversion))

  (check
      (stack->list (stack))
    => '())

  (check
      (stack->list (stack 1))
    => '(1))

  (check
      (stack->list (stack 1 2 3))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (stack->vector (stack))
    => '#())

  (check
      (stack->vector (stack 1))
    => '#(1))

  (check
      (stack->vector (stack 1 2 3))
    => '#(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (stack->list (list->stack '()))
    => '())

  (check
      (stack->list (list->stack '(1)))
    => '(1))

  (check
      (stack->list (list->stack '(1 2 3)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (stack->vector (vector->stack '#()))
    => '#())

  (check
      (stack->vector (vector->stack '#(1)))
    => '#(1))

  (check
      (stack->vector (vector->stack '#(1 2 3)))
    => '#(1 2 3))

  #t)



(check-report)

;;; end of file

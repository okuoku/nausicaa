;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: stack record definition
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


(library (stacks)
  (export
    <stack>		<stack-rtd>
    <stack*>		stack

    stack-empty?	stack-length

    stack-top
    stack-push!		stack-pop!

    stack-find		stack-for-all
    stack-exists

    stack-remp!		stack-remove!
    stack-remv!		stack-remq!
    stack-filter!

    stack-memp		stack-member
    stack-memv		stack-memq

    stack->list		list->stack
    stack->vector	vector->stack)
  (import (rnrs)
    (rnrs mutable-pairs)
    (stacks types)
    (stacks extensions))


;;; helpers

(define-syntax list-copy/stx
  (syntax-rules ()
    ((_ ?ell)
     (let loop ((ell ?ell))
       (if (pair? ell)
	   (cons (car ell) (loop (cdr ell)))
	 ell)))))


(define stack
  (case-lambda
   (()
    (make-<stack> '()))
   (args
    (make-<stack> args))))


(define (stack-push! obj stk)
  (assert (<stack>? stk))
  (let-syntax ((first-pair	(identifier-syntax (_ (<stack>-first-pair stk))
						   ((set! _ ?value)
						    (<stack>-first-pair-set! stk ?value)))))
    (set! first-pair (cons obj first-pair))))

(define (stack-pop! stk)
  (assert (<stack>? stk))
  (let-syntax ((first-pair	(identifier-syntax (_ (<stack>-first-pair stk))
						   ((set! _ ?value)
						    (<stack>-first-pair-set! stk ?value)))))
    (let ((first first-pair))
      (if (null? first)
	  (error 'stack-pop! "stack is empty" stk)
	(set! first-pair (cdr first)))
      (car first))))


(define (stack-find proc stk)
  (assert (<stack>? stk))
  (assert (procedure? proc))
  (find proc (<stack>-first-pair stk)))

(define (stack-for-all proc stk)
  (assert (<stack>? stk))
  (assert (procedure? proc))
  (for-all proc (<stack>-first-pair stk)))

(define (stack-exists proc stk)
  (assert (<stack>? stk))
  (assert (procedure? proc))
  (exists proc (<stack>-first-pair stk)))


(define (%remove remover thing stk)
  (let-syntax ((first-pair	(identifier-syntax (_ (<stack>-first-pair stk))
						   ((set! _ ?value)
						    (<stack>-first-pair-set! stk ?value)))))
    (set! first-pair (remover thing first-pair))))

(define (stack-remp! proc stk)
  (assert (<stack>? stk))
  (assert (procedure? proc))
  (%remove remp proc stk))

(define (stack-remove! obj stk)
  (assert (<stack>? stk))
  (%remove remove obj stk))

(define (stack-remv! obj stk)
  (assert (<stack>? stk))
  (%remove remv obj stk))

(define (stack-remq! obj stk)
  (assert (<stack>? stk))
  (%remove remq obj stk))

(define (stack-filter! proc stk)
  (assert (<stack>? stk))
  (assert (procedure? proc))
  (%remove filter proc stk))


(define (stack-memp proc stk)
  (assert (<stack>? stk))
  (assert (procedure? proc))
  (memp proc (<stack>-first-pair stk)))

(define (stack-member obj stk)
  (assert (<stack>? stk))
  (member obj (<stack>-first-pair stk)))

(define (stack-memv obj stk)
  (assert (<stack>? stk))
  (memv obj (<stack>-first-pair stk)))

(define (stack-memq obj stk)
  (assert (<stack>? stk))
  (memq obj (<stack>-first-pair stk)))


(define (stack->list stk)
  (assert (<stack>? stk))
  (list-copy/stx (<stack>-first-pair stk)))

(define (list->stack ell)
  (assert (list? ell))
  (make-<stack> (list-copy/stx ell)))

(define (stack->vector stk)
  (assert (<stack>? stk))
  (list->vector (<stack>-first-pair stk)))

(define (vector->stack vec)
  (assert (vector? vec))
  (make-<stack> (vector->list vec)))


;;;; done

)

;;; end of file

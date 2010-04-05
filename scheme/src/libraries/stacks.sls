;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: stack record definition
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


(library (stacks)
  (export
    <stack>		<stack>?	<stack>-with-record-fields-of
    make-<stack>
    <stack>-first-pair	<stack>-first-pair-set!

    stack-empty?	stack-length

    stack-top
    stack-push!		stack-pop!
    stack-purge!

    stack-find		stack-for-all
    stack-exists

    stack-remp!		stack-remove!
    stack-remv!		stack-remq!
    stack-filter!

    stack-memp		stack-member
    stack-memv		stack-memq

    stack->list		list->stack
    stack->vector	vector->stack)
  (import (nausicaa)
    (rnrs mutable-pairs))


;;; helpers

(define-inline (list-copy/stx ?ell)
  (let loop ((ell ?ell))
    (if (pair? ell)
	(cons (car ell) (loop (cdr ell)))
      ell)))


(define-class <stack>
  (protocol (lambda (make-<top>)
	      (case-lambda
	       (()
		((make-<top>) '()))
	       (args
		((make-<top>) args)))))
  (fields (mutable first-pair))
  (virtual-fields (immutable top	stack-top)
		  (immutable empty?	stack-empty?)
		  (immutable length	stack-length)))

(define (stack-top (S <stack>))
  (if (null? S.first-pair)
      (error 'stack-top "stack is empty" S)
    (car S.first-pair)))

(define (stack-empty? (S <stack>))
  (null? S.first-pair))

(define (stack-length (S <stack>))
  (length S.first-pair))


(define (stack-push! obj (S <stack>))
  (set! S.first-pair (cons obj S.first-pair)))

(define (stack-pop! (S <stack>))
  (if (null? S.first-pair)
      (error 'stack-pop! "stack is empty" S)
    (begin0-let ((v (car S.first-pair)))
      (set! S.first-pair (cdr S.first-pair)))))

(define (stack-purge! (S <stack>))
  (set! S.first-pair '()))


(define (stack-find proc (S <stack>))
  (find proc S.first-pair))

(define (stack-for-all proc (S <stack>))
  (for-all proc S.first-pair))

(define (stack-exists proc (S <stack>))
  (exists proc S.first-pair))


(define (%remove remover thing (S <stack>))
  (set! S.first-pair (remover thing S.first-pair)))

(define (stack-remp! proc stk)
  (%remove remp proc stk))

(define (stack-remove! obj stk)
  (%remove remove obj stk))

(define (stack-remv! obj stk)
  (%remove remv obj stk))

(define (stack-remq! obj stk)
  (%remove remq obj stk))

(define (stack-filter! proc stk)
  (%remove filter proc stk))


(define (stack-memp proc (S <stack>))
  (memp proc S.first-pair))

(define (stack-member obj (S <stack>))
  (member obj S.first-pair))

(define (stack-memv obj (S <stack>))
  (memv obj S.first-pair))

(define (stack-memq obj (S <stack>))
  (memq obj S.first-pair))


(define (stack->list (S <stack>))
  (list-copy/stx S.first-pair))

(define (list->stack ell)
  (apply make-<stack> ell))

(define (stack->vector (S <stack>))
  (list->vector S.first-pair))

(define (vector->stack vec)
  (apply make-<stack> (vector->list vec)))


;;;; done

)

;;; end of file

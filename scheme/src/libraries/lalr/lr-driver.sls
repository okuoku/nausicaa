;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: LALR(1) parser LR-driver
;;;Date: Tue Jul 21, 2009
;;;
;;;Abstract
;;;
;;;	This library  is a LALR(1)  parser generator written  in Scheme.
;;;	It implements an efficient algorithm for computing the lookahead
;;;	sets.  The  algorithm is the  same as used  in GNU Bison  and is
;;;	described in:
;;;
;;;	   F.  DeRemer  and  T.  Pennello.  ``Efficient  Computation  of
;;;	   LALR(1)  Look-Ahead Set''.   TOPLAS, vol.  4, no.  4, october
;;;	   1982.
;;;
;;;	As a consequence, it is not written in a fully functional style.
;;;	In fact,  much of  the code  is a direct  translation from  C to
;;;	Scheme of the Bison sources.
;;;
;;;	The library is  a port to @rnrs{6} Scheme of  Lalr-scm by .  The
;;;	original code is available at:
;;;
;;;			<http://code.google.com/p/lalr-scm/>
;;;
;;;Copyright (c) 2005-2008 Dominique Boucher
;;;Port to R6RS and Nausicaa integration by Marco Maggi.
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
(library (lalr lr-driver)
  (export lr-driver)
  (import (rnrs)
    (lalr common))



(define *max-stack-size* 500)



(define (lr-driver action-table goto-table reduction-table)
  (define ___atable action-table)
  (define ___gtable goto-table)
  (define ___rtable reduction-table)

  (define ___lexerp #f)
  (define ___errorp #f)

  (define ___stack  #f)
  (define ___sp     0)

  (define ___curr-input #f)
  (define ___reuse-input #f)

  (define ___input #f)
  (define (___consume)
    (set! ___input (if ___reuse-input ___curr-input (___lexerp)))
    (set! ___reuse-input #f)
    (set! ___curr-input ___input))

  (define (___pushback)
    (set! ___reuse-input #t))

  (define (___initstack)
    (set! ___stack (make-vector *max-stack-size* 0))
    (set! ___sp 0))

  (define (___growstack)
    (let ((new-stack (make-vector (* 2 (vector-length ___stack)) 0)))
      (let loop ((i (- (vector-length ___stack) 1)))
        (if (>= i 0)
	    (begin
	      (vector-set! new-stack i (vector-ref ___stack i))
	      (loop (- i 1)))))
      (set! ___stack new-stack)))

  (define (___checkstack)
    (if (>= ___sp (vector-length ___stack))
        (___growstack)))

  (define (___push delta new-category lvalue)
    (set! ___sp (- ___sp (* delta 2)))
    (let* ((state     (vector-ref ___stack ___sp))
           (new-state (cdr (assoc new-category (vector-ref ___gtable state)))))
      (set! ___sp (+ ___sp 2))
      (___checkstack)
      (vector-set! ___stack ___sp new-state)
      (vector-set! ___stack (- ___sp 1) lvalue)))

  (define (___reduce st)
    ((vector-ref ___rtable st) ___stack ___sp ___gtable ___push ___pushback))

  (define (___shift token attribute)
    (set! ___sp (+ ___sp 2))
    (___checkstack)
    (vector-set! ___stack (- ___sp 1) attribute)
    (vector-set! ___stack ___sp token))

  (define (___action x l)
    (let ((y (assoc x l)))
      (if y (cadr y) (cadar l))))

  (define (___recover tok)
    (let find-state ((sp ___sp))
      (if (< sp 0)
          (set! ___sp sp)
	(let* ((state (vector-ref ___stack sp))
	       (act   (assoc 'error (vector-ref ___atable state))))
	  (if act
	      (begin
		(set! ___sp sp)
		(___sync (cadr act) tok))
	    (find-state (- sp 2)))))))

  (define (___sync state tok)
    (let ((sync-set (map car (cdr (vector-ref ___atable state)))))
      (set! ___sp (+ ___sp 4))
      (___checkstack)
      (vector-set! ___stack (- ___sp 3) #f)
      (vector-set! ___stack (- ___sp 2) state)
      (let skip ()
        (let ((i (___category ___input)))
          (if (eq? i '*eoi*)
              (set! ___sp -1)
	    (if (memq i sync-set)
		(let ((act (assoc i (vector-ref ___atable state))))
		  (vector-set! ___stack (- ___sp 1) #f)
		  (vector-set! ___stack ___sp (cadr act)))
	      (begin
		(___consume)
		(skip))))))))

  (define (___category tok)
    (if (lexical-token? tok)
        (lexical-token-category tok)
      tok))

  (define (___value tok)
    (if (lexical-token? tok)
        (lexical-token-value tok)
      tok))

  (define (___run)
    (let loop ()
      (if ___input
          (let* ((state (vector-ref ___stack ___sp))
                 (i     (___category ___input))
                 (attr  (___value ___input))
                 (act   (___action i (vector-ref ___atable state))))
            (cond ((not (symbol? i))
                   (___errorp "syntax error: invalid token: " ___input)
                   #f)

                  ;;Input  succesfully parsed.   This happens  also when
                  ;;"*eoi*" is  found, which  means that the  value from
                  ;;the  vector is  returned to  the caller;  this value
                  ;;should be #<unspecified>.
                  ((eq? act 'accept)
                   (vector-ref ___stack 1))

                  ;;Syntax error in input.
                  ((eq? act '*error*)
                   (if (eq? i '*eoi*)
                       (begin
                         (___errorp "syntax error: unexpected end of input")
                         #f)
		     (begin
		       (___errorp "syntax error: unexpected token: " ___input)
		       (___recover i)
		       (if (>= ___sp 0)
			   (set! ___input #f)
			 (begin
			   (set! ___sp 0)
			   (set! ___input '*eoi*)))
		       (loop))))

                  ;;Shift current token on top of the stack.
                  ((>= act 0)
                   (___shift act attr)
		   (set! ___input (if (eq? i '*eoi*) '*eoi* #f))
                   (loop))

                  ;; Reduce by rule (- act)
                  (else
                   (___reduce (- act))
                   (loop))))

	;; no lookahead, so check if there is a default action
	;; that does not require the lookahead
	(let* ((state  (vector-ref ___stack ___sp))
	       (acts   (vector-ref ___atable state))
	       (defact (if (pair? acts) (cadar acts) #f)))
	  (if (and (= 1 (length acts)) (< defact 0))
	      (___reduce (- defact))
	    (___consume))
	  (loop)))))

  (lambda (lexerp errorp)
    (set! ___errorp errorp)
    (set! ___lexerp lexerp)
    (___initstack)
    (___run)))


;;;; done

)

;;; end of file

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
(library (lalr glr-driver)
  (export
    glr-driver

    ;; re-exports from (lalr common)
    make-source-location	source-location?
    source-location-line
    source-location-input
    source-location-column
    source-location-offset
    source-location-length

    make-lexical-token		lexical-token?
    lexical-token-value
    lexical-token-category
    lexical-token-source

    lexical-token?/end-of-input)
  (import (rnrs)
    (lalr common))

(define-syntax drop/stx
  (syntax-rules ()
    ((_ ?ell ?k)
     (let loop ((ell ?ell)
		(k   ?k))
       (if (zero? k)
	   ell
	 (loop (cdr ell) (- k 1)))))))

(define-syntax take-right/stx
  (syntax-rules ()
    ((_ ?ell ?k)
     (let ((ell ?ell))
       (let loop ((lag	ell)
		  (lead	(drop/stx ell ?k)))
	 (if (pair? lead)
	     (loop (cdr lag) (cdr lead))
	   lag))))))


(define (glr-driver action-table goto-table reduction-table)
  (define ___atable action-table)
  (define ___gtable goto-table)
  (define ___rtable reduction-table)

  (define ___lexerp #f)
  (define ___errorp #f)

  ;; -- Input handling

  (define *input* #f)
  (define (initialize-lexer lexer)
    (set! ___lexerp lexer)
    (set! *input* #f))
  (define (consume)
    (set! *input* (___lexerp)))

  (define (token-category tok)
    (if (lexical-token? tok)
        (lexical-token-category tok)
      tok))

  (define (token-attribute tok)
    (if (lexical-token? tok)
        (lexical-token-value tok)
      tok))

  ;; -- Processes (stacks) handling

  (define *processes* '())

  (define (initialize-processes)
    (set! *processes* '()))
  (define (add-process process)
    (set! *processes* (cons process *processes*)))
  (define (get-processes)
    (reverse *processes*))

  (define (for-all-processes proc)
    (let ((processes (get-processes)))
      (initialize-processes)
      (for-each proc processes)))

  ;; -- parses
  (define *parses* '())
  (define (get-parses)
    *parses*)
  (define (initialize-parses)
    (set! *parses* '()))
  (define (add-parse parse)
    (set! *parses* (cons parse *parses*)))


  (define (push delta new-category lvalue stack)
    (let* ((stack     (drop/stx stack (* delta 2)))
           (state     (car stack))
           (new-state (cdr (assv new-category (vector-ref ___gtable state)))))
      (cons new-state (cons lvalue stack))))

  (define (reduce state stack)
    ((vector-ref ___rtable state) stack ___gtable push))

  (define (shift state symbol stack)
    (cons state (cons symbol stack)))

  (define (get-actions token action-list)
    (let ((pair (assoc token action-list)))
      (if pair
          (cdr pair)
	(cdar action-list)))) ;; get the default action


  (define (run)
    (let loop-tokens ()
      (consume)
      (let ((symbol (token-category *input*))
            (attr   (token-attribute *input*)))
        (for-all-processes
         (lambda (process)
           (let loop ((stacks (list process)) (active-stacks '()))
             (cond ((pair? stacks)
                    (let* ((stack   (car stacks))
                           (state   (car stack)))
                      (let actions-loop ((actions      (get-actions symbol (vector-ref ___atable state)))
                                         (active-stacks active-stacks))
                        (if (pair? actions)
                            (let ((action        (car actions))
                                  (other-actions (cdr actions)))
                              (cond ((eq? action '*error*)
                                     (actions-loop other-actions active-stacks))
                                    ((eq? action 'accept)
                                     (add-parse (car (take-right/stx stack 2)))
                                     (actions-loop other-actions active-stacks))
                                    ((>= action 0)
                                     (let ((new-stack (shift action attr stack)))
                                       (add-process new-stack))
                                     (actions-loop other-actions active-stacks))
                                    (else
                                     (let ((new-stack (reduce (- action) stack)))
				       (actions-loop other-actions (cons new-stack active-stacks))))))
			  (loop (cdr stacks) active-stacks)))))
                   ((pair? active-stacks)
                    (loop (reverse active-stacks) '())))))))
      (if (pair? (get-processes))
          (loop-tokens))))


  (lambda (lexerp errorp yycustom)
    (set! ___errorp errorp)
    (initialize-lexer lexerp)
    (initialize-processes)
    (initialize-parses)
    (add-process '(0))
    (run)
    (get-parses)))


;;;; done

)

;;; end of file

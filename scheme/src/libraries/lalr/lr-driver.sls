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
  (export
    lr-driver

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
    lexical-token-source)
  (import (rnrs)
    (lalr common)
    (parameters)
    (checks))

(define-syntax drop/stx
  (syntax-rules ()
    ((_ ?ell ?k)
     (let loop ((ell ?ell)
		(k   ?k))
       (if (zero? k)
	   ell
	 (loop (cdr ell) (- k 1)))))))


(define (lr-driver action-table goto-table reduction-table)
  (lambda (true-lexer error-handler yycustom)
    (let ((stack-values		'(#f))
	  (stack-states		'(0))
	  (reuse-last-token	#f))

      (define (main lookahead)
	(let* ((state-index	(car stack-states))
	       (category	(lexical-token-category lookahead))
	       (action-value	(select-action category state-index)))
	  (debug "~%main states ~s values ~s state= ~s category= ~s action ~s"
		 stack-states stack-values state-index category action-value)
	  (cond
	   ((eq? action-value 'accept) ;success, return the value to the caller
	    (cadr stack-values))

	   ((eq? action-value '*error*) ;syntax error in input
	    (if (eq? category '*eoi*)
		(error-handler "unexpected end of input" lookahead)
	      (begin
		(error-handler "syntax error, unexpected token" lookahead)
		(let-values (((success token) (recover-from-error lookahead)))
		  (when success
		    (reduce-using-default-actions))
		  ;;If recovery fails, TOKEN is set to end--of--input
		  (main token)))))

	   ((<= 0 action-value) ;shift (= push) token on the stack
	    (debug "shift: ~s new-state= ~s" lookahead action-value)
	    (stack-push! (lexical-token-value lookahead) action-value)
	    (main (if (eq? category '*eoi*)
		      (begin
			(reduce-using-default-actions)
			lookahead)
		    (lexer))))

	   (else ;reduce using the rule at index "(- ACTION-VALUE)"
	    (debug "reduce action-value= ~a" action-value)
	    (reduce (- action-value))
	    (main lookahead))))) ;we have not used the token, yet

      (define lexer
	(let ((last-token #f))
	  (lambda ()
	    (if reuse-last-token
		(set! reuse-last-token #f)
	      (begin
		(set! last-token (true-lexer))
		(unless (lexical-token? last-token)
		  (error-handler "expected lexical token from lexer" last-token)
		  (true-lexer))))
	    (debug "lookahead ~s" last-token)
	    last-token)))

      (define (yypushback)
	(set! reuse-last-token #t))

      (define (select-action terminal-symbol state-index)
	(let* ((action-alist (vector-ref action-table state-index))
	       (pair (assq terminal-symbol action-alist)))
	  (if pair (cdr pair) (cdar action-alist))))

      (define (reduce reduction-table-index)
	(debug "reduce index ~s" reduction-table-index)
	(apply (vector-ref reduction-table reduction-table-index)
	       reduce-pop-and-push yypushback yycustom stack-values))

      (define (reduce-pop-and-push used-values goto-keyword semantic-action-result yy-stack-values)
	(debug "pop-and-push states ~s values ~s used ~s goto ~s"
	       stack-states yy-stack-values used-values goto-keyword)
	(set! stack-states (drop/stx stack-states used-values))
	(set! stack-values yy-stack-values)
	(let ((new-state-index (cdr (assq goto-keyword
					  (vector-ref goto-table (car stack-states))))))
	  (debug "after-reduce old-state ~s goto-keyword ~s new-state ~s "
		 (car stack-states) goto-keyword new-state-index)
	  (set! stack-values (cons semantic-action-result stack-values))
	  (set! stack-states (cons new-state-index stack-states))))

      (define (reduce-using-default-actions)
	(debug "reducing-default states ~s values ~s state ~s"
	       stack-states stack-values (car stack-states))
	(let ((actions-alist (vector-ref action-table (car stack-states))))
	  (when (= 1 (length actions-alist))
;;;(assert (eq? '*default* (caar actions-alist)))
            (let ((default-action (cdar actions-alist)))
	      (when (< default-action 0)
		(reduce (- default-action))
		(reduce-using-default-actions))))))

      (define (recover-from-error offending-token)
        (let rewind-stack-loop ()
	  (debug "recovering rewind states ~s values ~s offending-token ~s"
		 stack-states stack-values offending-token)
	  (if (null? stack-values)
	      (begin
		(stack-push! #f 0)
		(values #f (make-lexical-token '*eoi* ;recovery failed, simulate end-of-input
					       (lexical-token-source offending-token)
					       (eof-object))))
	    (let* ((error-entry (assq 'error (vector-ref action-table (car stack-states)))))
	      (if error-entry
		  (synchronise-stack-and-input offending-token (cdr error-entry))
		(begin
		  (set! stack-values (cdr stack-values))
		  (set! stack-states (cdr stack-states))
		  (rewind-stack-loop)))))))

       (define (synchronise-stack-and-input offending-token error-state-index)
	 (debug "recovering-sync states ~s values ~s error-state-index ~s"
		stack-states stack-values error-state-index)
         (let* ((error-actions	(vector-ref action-table error-state-index))
		(sync-set	(map car (cdr error-actions))))
	   (let skip-token ((token offending-token))
	     (let ((category (lexical-token-category token)))
	       (cond ((eq? category '*eoi*) ;unexpected end-of-input while trying to recover
		      (values #f token))
		     ((memq category sync-set) ;recovery success
		      ;;The following  stack entry will  be processed by
		      ;;REDUCE-USING-DEFAULT-ACTIONS,     causing    the
		      ;;evaluation  of  the   semantic  action  for  the
		      ;;"error" right-hand side rule.
		      (stack-push! #f #f)
		      (stack-push! #f (cdr (assq category error-actions)))
		      (values #t token))
		     (else
		      (skip-token (lexer))))))))

      (define-syntax stack-push! (syntax-rules ()
				   ((_ ?value ?state)
				    (begin
				      (set! stack-values (cons ?value stack-values))
				      (set! stack-states (cons ?state stack-states))))))

      (main (lexer)))))


;;;; done

)

;;; end of file

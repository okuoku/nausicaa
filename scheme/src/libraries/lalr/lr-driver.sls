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
    lr-driver			lalr-initial-stack-size

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
    (lalr common)
    (parameters))

(define lalr-initial-stack-size
  (make-parameter 500))


(define (lr-driver action-table goto-table reduction-table)
  (lambda (lexer error-handler yycustom)
    (let ((stack		(make-vector (lalr-initial-stack-size) 0))
	  (stack-pointer	0)
	  (reuse-last-token	#f))

      (define (main lookahead-token)
	(let* ((state-index	(stack-ref stack-pointer))
	       (category	(lexical-token-category lookahead-token))
	       (action-value	(select-action category state-index)))
;;;(clean-stack)
;;;(write (list 'main stack-pointer stack action-value))(newline)
;;;(write (list 'token category action-value))(newline)
	  (cond
	   ((eq? action-value 'accept) ;success, return the value to the caller
	    (stack-ref 1))

	   ((eq? action-value '*error*) ;syntax error in input
	    (if (eq? category '*eoi*)
		(error-handler "unexpected end of input" lookahead-token)
	      (begin
		(error-handler "syntax error, unexpected token" lookahead-token)
		(let ((token (recover-from-error lookahead-token)))
		  (if (<= 0 stack-pointer)
		      (reduce-using-default-actions) ;recovery succeeded
		    (set! stack-pointer 0)) ;recovery failed, TOKEN set to end--of--input
		  (main token)))))

	   ((<= 0 action-value) ;shift (= push) token on the stack
;;;(write (list 'shift lookahead-token))(newline)
	    (stack-push! (lexical-token-value lookahead-token) action-value)
	    (main (if (eq? category '*eoi*)
		      (begin
			(reduce-using-default-actions)
			lookahead-token)
		    (retrieve-token-from-lexer))))

	   (else ;reduce using the rule at index "(- ACTION-VALUE)"
;;;(write (list 'reduce action-value))(newline)
	    (reduce (- action-value))
	    (main lookahead-token))))) ;we have not used the token, yet

      (define (clean-stack)
        ;;To be used for debugging  only.  Clears the unused portions of
        ;;the stack.
        ;;
        (do ((i (+ 1 stack-pointer) (+ 1 i)))
            ((>= i (vector-length stack)))
          (vector-set! stack i #f)))

      (define-syntax stack-set!	(syntax-rules ()
				  ((_ ?offset ?value)
				   (vector-set! stack ?offset ?value))))

      (define-syntax stack-ref	(syntax-rules ()
				  ((_ ?offset)
				   (vector-ref stack ?offset))))

      (define-syntax action-ref	(syntax-rules ()
				  ((_ ?state)
				   (vector-ref action-table ?state))))

      (define-syntax increment!	(syntax-rules ()
				  ((_ ?varname ?step)
				   (set! ?varname (+ ?varname ?step)))))

      (define-syntax stack-pop!	(syntax-rules ()
				  ((_ ?used-couples)
				   (increment! stack-pointer (* -2 ?used-couples)))))

      (define-syntax stack-push! (syntax-rules ()
				   ((_ ?first ?second)
				    (begin
				      (increment! stack-pointer 2)
				      (enlarge-stack-if-needed)
				      (stack-set! (- stack-pointer 1) ?first)
				      (stack-set! stack-pointer       ?second)))))

      (define (enlarge-stack-if-needed)
	(let ((len (vector-length stack)))
	  (when (>= stack-pointer len)
	    (do ((new-stack (make-vector (* 2 len) 0))
		 (i 0 (+ 1 i)))
		((= i len)
		 (set! stack new-stack))
	      (vector-set! new-stack i (stack-ref i))))))

      (define retrieve-token-from-lexer
	(let ((last-token #f))
	  (lambda ()
	    (if reuse-last-token
		(set! reuse-last-token #f)
	      (begin
		(set! last-token (lexer))
		(unless (lexical-token? last-token)
		  (error-handler "expected lexical token from lexer" last-token)
		  (retrieve-token-from-lexer))))
;;;(write (list 'lookahead last-token))(newline)
	    last-token)))

      (define (yypushback)
	(set! reuse-last-token #t))

      (define (select-action terminal-symbol state-index)
	(let* ((action-alist (action-ref state-index))
	       (pair (assq terminal-symbol action-alist)))
	  (if pair (cdr pair) (cdar action-alist))))

      (define (reduce reduction-table-index)
	((vector-ref reduction-table reduction-table-index)
	 stack stack-pointer reduce-pop-and-push yypushback yycustom))

      (define (reduce-pop-and-push used-couples goto-keyword client-form-result-value)
	(stack-pop! used-couples)
	(let* ((state-index	(stack-ref stack-pointer))
	       (new-state-index	(cdr (assq goto-keyword
					   (vector-ref goto-table state-index)))))
	  (stack-push! client-form-result-value new-state-index)))

      (define (reduce-using-default-actions)
	(let* ((state-index	(stack-ref  stack-pointer))
	       (actions-alist	(action-ref state-index)))
	  (when (= 1 (length actions-alist))
            ;;;(assert (eq? '*default* (caar actions-alist)))
            (let ((default-action (cdar actions-alist)))
	      (when (< default-action 0)
;;;(write (list 'reducing-default default-action stack))(newline)
		(reduce (- default-action))
		(reduce-using-default-actions))))))

      (define (recover-from-error offending-token)
;;;(write (list 'recovering offending-token stack))(newline)
        (let rewind-stack-loop ()
	  (if (< stack-pointer 0)
	      (make-lexical-token '*eoi* ;recovery failed, simulate end-of-input
				  (lexical-token-source offending-token)
				  (eof-object))
	    (let* ((action-index (stack-ref stack-pointer))
		   (error-entry  (assq 'error (action-ref action-index))))
	      (if (not error-entry)
		  (begin
		    (increment! stack-pointer -2)
		    (rewind-stack-loop))
		(synchronise-stack-and-input offending-token (cdr error-entry)))))))

       (define (synchronise-stack-and-input offending-token error-state-index)
;;;(write (list 'recover-sync-index error-state-index))(newline)
         (let* ((error-actions	(action-ref error-state-index))
		(sync-set	(map car (cdr error-actions))))
	   (increment! stack-pointer 4)
	   (enlarge-stack-if-needed)
	   (stack-set! (- stack-pointer 3) #f)
	   (stack-set! (- stack-pointer 2) #f)
;;;	   (stack-set! (- stack-pointer 2) error-state-index)
;;;(write (list 'recover-sync offending-token stack))(newline)
	   (let skip-token ((token offending-token))
	     (let ((category (lexical-token-category token)))
	       (cond ((eq? category '*eoi*) ;end-of-input while trying to recover
		      (set! stack-pointer -1)
		      token)
		     ((memq category sync-set) ;
		      ;;The following  stack entry will  be processed by
		      ;;REDUCE-USING-DEFAULT-ACTIONS,     causing    the
		      ;;evaluation  of  the   semantic  action  for  the
		      ;;"error" right-hand side rule.
		      (stack-set! (- stack-pointer 1) #f)
		      (stack-set! stack-pointer (cdr (assq category error-actions)))
;;;(write (list 'recover-finish offending-token stack))(newline)
		      token)
		     (else
		      (skip-token (retrieve-token-from-lexer))))))))

      (main (retrieve-token-from-lexer)))))


;;;; done

)

;;; end of file

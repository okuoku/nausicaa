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

(define eoi-token
  (make-lexical-token '*eoi*
		      (make-source-location #f +nan.0 +nan.0 +nan.0 0)
		      (eof-object)))


(define (lr-driver action-table goto-table reduction-table)
  (lambda (lexer error-handler)
    (let ((stack		(make-vector (lalr-initial-stack-size) 0))
	  (stack-pointer	0)
	  (reuse-last-token	#f))

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

      (define consume
	(let ((last-token #f))
	  (lambda ()
	    (if reuse-last-token
		(set! reuse-last-token #f)
	      (begin
		(set! last-token (lexer))
		(unless (lexical-token? last-token)
		  (error-handler "expected lexical token from lexer" last-token)
		  (consume))))
	    last-token)))

      (define (yypushback)
	(set! reuse-last-token #t))

      (define (select-action terminal-symbol state-index)
	(let* ((action-alist (action-ref state-index))
	       (pair (assq terminal-symbol action-alist)))
	  (if pair (cdr pair) (cdar action-alist))))

      (define (reduce reduction-index)
	((vector-ref reduction-table reduction-index)
	 stack stack-pointer reduce-pop-and-push yypushback))

      (define (reduce-pop-and-push used-couples goto-keyword client-form-result-value)
	(stack-pop! used-couples)
	(let* ((state-index	(stack-ref stack-pointer))
	       (new-state-index	(cdr (assq goto-keyword
					   (vector-ref goto-table state-index)))))
	  (stack-push! client-form-result-value new-state-index)))

      (define (reduce-using-default-action)
	(let* ((state-index	(stack-ref  stack-pointer))
	       (actions-alist	(action-ref state-index))
	       (default-action	(and (pair? actions-alist)
				     (cdar actions-alist))))
	  ;;(assert (eq? '*default* (caar actions-alist)))
	  (when (and (= 1 (length actions-alist))
		     (< default-action 0))
	    (reduce (- default-action))
	    (reduce-using-default-action))))

      (define (recover-from-error offending-token)
	;;Rewind the stack looking  for an action index whose action
	;;alist has  an "error" field.   If it finds one  call SYNC,
	;;else return leaving STACK-POINTER set to -1.
	;;
	(let rewind-stack ()
	  (if (< stack-pointer 0)
		eoi-token ;recovery failed, simulate end-of-input
	    (let* ((action-index (stack-ref stack-pointer))
		   (error-entry  (assq 'error (action-ref action-index))))
	      (if (not error-entry)
		  (begin
		    (increment! stack-pointer -2)
		    (rewind-stack))
		(let* ((state-index (cdr error-entry))
		       ;;SYNC-SET is  the list of keywords  in the error
		       ;;action alist, with the exclusion of the default
		       ;;entry.
		       (sync-set    (map car (cdr (action-ref state-index)))))
		  (increment! stack-pointer 4)
		  (enlarge-stack-if-needed)
		  (stack-set! (- stack-pointer 3) #f)
		  (stack-set! (- stack-pointer 2) state-index)
		  (let skip-token ((token offending-token))
		    (let ((category (lexical-token-category token)))
		      (if (eq? category '*eoi*)
			  (begin ;end-of-input while trying to recover, return end-of-input token
			    (set! stack-pointer -1)
			    token)
			(if (memq category sync-set)
			    (let ((act (assq category (action-ref state-index))))
			      (stack-set! (- stack-pointer 1) #f)
			      (stack-set! stack-pointer (cdr act))
			      token)
			  (skip-token (consume))))))))))))

      (let loop ((token (consume)))
	(let* ((state-index	(stack-ref stack-pointer))
	       (category	(lexical-token-category token))
	       (action-value	(select-action category state-index)))
	  (cond

	   ((eq? action-value 'accept) ;success, return the value to the caller
	    (stack-ref 1))

	   ((eq? action-value '*error*) ;syntax error in input
	    (if (eq? category '*eoi*)
		(error-handler "unexpected end of input" token)
	      (begin
		(error-handler "syntax error, unexpected token" token)
		(let ((token (recover-from-error token)))
		  (if (<= 0 stack-pointer)
		      (reduce-using-default-action) ;recovery succeeded
		    (set! stack-pointer 0));recovery failed, TOKEN set to end--of--input
		  (loop token)))))

	   ((>= action-value  0) ;shift current token on top of the stack
	    (stack-push! (lexical-token-value token) action-value)
	    (if (eq? category '*eoi*)
		(set! token eoi-token)
	      (reduce-using-default-action))
	    (loop (consume)))

	   (else ;reduce by rule (- action-value)
	    (reduce (- action-value))
	    (loop token)))));end of LOOP
      )))


;;;; done

)

;;; end of file

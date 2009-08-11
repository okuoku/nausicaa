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
    (checks)
    (lalr common))


;;;; helpers

(define-syntax drop/stx
  (syntax-rules ()
    ((_ ?ell ?k)
     (let loop ((ell ?ell)
		(k   ?k))
       (if (zero? k)
	   ell
	 (loop (cdr ell) (- k 1)))))))

(define-syntax receive
  (syntax-rules ()
    ((_ formals expression b b* ...)
     (call-with-values
         (lambda () expression)
       (lambda formals b b* ...)))))


;;;; process utilities
;;
;;A "process" is  a couple of stacks, one for the  state numbers and one
;;for the  value numbers.  Every time  a shift action  is performed, one
;;value is  pushed on both  the stacks.  Every  time a reduce  action is
;;performed, values from both the stacks are removed and replaced with a
;;single value.
;;
;;Processes are implemented  as pairs of lists; the CAR  is the stack of
;;states, the CDR is the stack of values.
;;
;;Some  of the  following definitions  are  actually used  in the  code;
;;others are here only for reference.
;;

(define make-process cons)
(define process-states-ref car)
(define process-values-ref cdr)

(define process-top-state caar)
(define process-top-value cadr)

(define-syntax process-push
  (syntax-rules ()
    ((_ ?process ?state ?value)
     (let ((process ?process))
       (cons (cons ?state (process-states-ref process))
	     (cons ?value (process-values-ref process)))))))



(define (glr-driver action-table goto-table reduction-table)
  (lambda (true-lexer error-handler yycustom)
    (define reuse-last-token #f)

    (define (main lookahead processes shifted results)
      (debug "main processes ~s" processes)
      (let-values (((shifted* results) (process->shifted lookahead (car processes) results)))
	(let ((processes (cdr processes))
	      (shifted   (append shifted shifted*)))
	  (debug "main: processes ~s shifted ~s" processes shifted)
	  (if (null? processes)
	      (if (null? shifted)
		  results
		(main (lexer) shifted '() results))
	    (main lookahead processes shifted results)))))

    (define (process->shifted lookahead process results)
      ;;Perform  the  actions upon  PROCESS  until  he  and its  spawned
      ;;processes  are  all accepted,  terminated  because  of error  or
      ;;shifted.
      ;;
      (let reduce-loop ((reduced (list process))
			(shifted '())
			(results results))
	(debug "process->shifted: reduced ~s, shifted ~s, results ~s" reduced shifted results)
	(if (null? reduced)
	    (values shifted results)
	  (receive (reduced shifted results)
	      (perform-actions lookahead (car reduced) results)
	    (reduce-loop reduced shifted results)))))

    (define (perform-actions lookahead process results)
      (debug "perform-actions: process ~s actions ~s" process
	     (select-actions lookahead (caar process)))
      (do ((action-list (select-actions lookahead (process-top-state process)) (cdr action-list))
	   (reduced '())
	   (shifted '()))
	  ((null? action-list)
	   (debug "perform-actions results: ~s ~s ~s" reduced shifted results)
	   (values reduced shifted results))
	(let ((action (car action-list)))
	  (debug "action ~s" action)
	  (cond ((eq? action '*error*) ;error, discard this process
		 #f)
		((eq? action 'accept) ;accept, register result and discard process
		 (set! results (cons (caar process) results)))
		((>= action 0) ;shift, this process survives
		 (set! shifted
		       (cons `((,action . ,(car process)) .
			       (,(lexical-token-value lookahead) . ,(cdr process)))
			     shifted)))
		(else ;reduce, this process will stay in the loop
		 (set! reduced
		       (cons (reduce (- action) process)
			     reduced)))))))

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

    (define (reduce state process)
      (apply (vector-ref reduction-table state)
	     reduce-pop-and-push yypushback yycustom (car process) (cdr process)))

    (define (reduce-pop-and-push used-values goto-keyword semantic-clause-result
				 stack-states stack-values)
      (let* ((stack-states (drop/stx stack-states used-values))
	     (state        (car stack-states))
	     (new-state    (cdr (assv goto-keyword (vector-ref goto-table state)))))
	(debug "after reduce: new-state ~s, goto-keyword ~s ~s ~s"
	       new-state goto-keyword state (vector-ref goto-table state))
	`((,new-state . ,stack-states) .
	  (,semantic-clause-result . ,stack-values))))

    (define (select-actions lookahead state-index)
      (let* ((action-alist (vector-ref action-table state-index))
	     (pair         (assq (lexical-token-category lookahead) action-alist)))
	(if pair (cdr pair) (cdar action-alist))))

    (main (lexer)
	  ;;List of  processes, each process  is a pair of  stacks.  The
	  ;;car is the stack of states, the cdr is the stack of values.
	  '( ((0) .  (#f)) )
	  '()
	  '())))


;;;; done

)

;;; end of file

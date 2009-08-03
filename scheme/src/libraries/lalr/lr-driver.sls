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
    lexical-token-source)
  (import (rnrs)
    (lalr common)
    (parameters))

(define lalr-initial-stack-size
  (make-parameter 500))


(define (lr-driver action-table goto-table reduction-table)
  (lambda (lexer error-handler)
;;;
;;;Notes:
;;;
;;;A value  returned by (lexer) can be  a Scheme symbol or  a record of
;;;type "lexical-token".  This value is stored in the INPUT variable.
;;;
;;;The stack
;;;---------
;;;
;;;Processing  of input  and rules  is performed  on a  stack:  A Scheme
;;;vector  which  is  dynamically  enlarged  when  needed.   The  "stack
;;;pointer" is an index in the stack's vector.  Values are pushed to and
;;;popped from the stack in couples.
;;;
;;;Couples  are  pushed  by  the  local functions:  PUSH,  SHIFT,  SYNC.
;;;Pushing a couple goes like this:
;;;
;;;	(increment! stack-pointer 2)
;;;	(enlarge-stack-if-needed)
;;;	(vector-set! stack (- stack-pointer 1) value)
;;;	(vector-set! stack stack-pointer       meta)
;;;
;;;this operation is implemented in  the STACK-PUSH!  macro.  META is an
;;;integer  describing a  parser state  or value  attribute, VALUE  is a
;;;token or the result of a reduction closure.
;;;
;;;Couples are popped only  by the local function POP-AND-PUSH.  Popping
;;;a single couple goes like this:
;;;
;;;	(increment! stack-pointer -2)
;;;
;;;popping a number of couples equal to USED-COUPLE goes like this:
;;;
;;;	(increment! stack-pointer (* -2 USED-COUPLES))
;;;
;;;this operation is implemented in the STACK-POP! macro.
;;;
;;;The reduction table
;;;-------------------
;;;
;;;REDUCTION-TABLE is  a vector of  closures embedding the  client forms
;;;coming from the parser rules.
;;;
;;;Each  closure takes  values from  the  stack, assigns  them to  local
;;;variables $1,  $2, ...   and evaluates the  client form.   The client
;;;form  is supposed  to use  the variables  $1, $2,  ...  to  compute a
;;;result.
;;;
;;;The values  used from  the stack  are popped, and  the result  of the
;;;client form is pushed.  This is the "reduction" process.
;;;
    (let ((stack		(make-vector (lalr-initial-stack-size) 0))
	  (stack-pointer	0)
	  (reuse-last-token	#f)
	  (input		#f))

      (let-syntax ((stack-set!	(syntax-rules ()
				  ((_ ?offset ?value)
				   (vector-set! stack ?offset ?value))))
		   (stack-ref	(syntax-rules ()
				  ((_ ?offset)
				   (vector-ref stack ?offset))))
		   (action-ref	(syntax-rules ()
				  ((_ ?state)
				   (vector-ref action-table ?state))))
		   (increment!	(syntax-rules ()
				  ((_ ?varname ?step)
				   (set! ?varname (+ ?varname ?step))))))
	(define-syntax stack-pop!
	  (syntax-rules ()
	    ((_ ?used-values)
	     (increment! stack-pointer (* -2 ?used-values)))))

	(define-syntax stack-push!
	  (syntax-rules ()
	    ((_ ?first ?second)
	     (begin
	       (increment! stack-pointer 2)
	       (enlarge-stack-if-needed)
	       (stack-set! (- stack-pointer 1) ?first)
	       (stack-set! stack-pointer       ?second)))))

	(define consume
	  ;;Consume a token from the input, or reuse the last token.
	  ;;
	  (let ((last-token #f))
	    (lambda ()
	      (set! input (if reuse-last-token
			      (begin
				(set! reuse-last-token #f)
				last-token)
			    (lexer)))
	      (set! last-token input))))

	(define (pushback)
	  ;;Signal to CONSUME that it has to reuse the last token.
	  ;;
	  (set! reuse-last-token #t))

	(define (category-ref tok)
	  (if (lexical-token? tok)
	      (lexical-token-category tok)
	    tok))

	(define (value-ref tok)
	  (if (lexical-token? tok)
	      (lexical-token-value tok)
	    tok))

	(define (enlarge-stack-if-needed)
	  ;;If the stack is full, double its size.
	  ;;
	  (let ((len (vector-length stack)))
	    (when (>= stack-pointer len)
	      (do ((new-stack (make-vector (* 2 len) 0))
		   (i 0 (+ 1 i)))
		  ((= i len)
		   (set! stack new-stack))
		(vector-set! new-stack i (stack-ref i))))))

	(define (pop-and-push used-values new-category lvalue)
	  ;;Called at the end of each reduction closure.
	  ;;
	  ;;Pop USED-VALUES couples from  the stack.  USED-VALUES can be
	  ;;zero.
	  ;;
	  ;;Push a  new couple: the new  state and LVALUE,  which is the
	  ;;value returned by the client form in the reduction closure.
	  ;;
	  (stack-pop! used-values)
	  (let* ((state     (stack-ref stack-pointer))
		 (new-state (cdr (assoc new-category (vector-ref goto-table state)))))
	    (stack-push! lvalue new-state)))

	(define (reduce st)
	  ((vector-ref reduction-table st) stack stack-pointer goto-table pop-and-push pushback))

	(define (action x l)
	  (let ((y (assoc x l)))
	    (if y (cadr y) (cadar l))))

	(define (recover tok)
	  (let find-state ((sp stack-pointer))
	    (if (< sp 0)
		(set! stack-pointer sp)
	      (let* ((state (stack-ref sp))
		     (act   (assoc 'error (action-ref state))))
		(if act
		    (begin
		      (set! stack-pointer sp)
		      (sync (cadr act) tok))
		  (find-state (- sp 2)))))))

	(define (sync state tok)
	  (let ((sync-set (map car (cdr (action-ref state)))))
	    (increment! stack-pointer 4)
	    (enlarge-stack-if-needed)
	    (stack-set! (- stack-pointer 3) #f)
	    (stack-set! (- stack-pointer 2) state)
	    (let skip ()
	      (let ((i (category-ref input)))
		(if (eq? i '*eoi*)
		    (set! stack-pointer -1)
		  (if (memq i sync-set)
		      (let ((act (assoc i (action-ref state))))
			(stack-set! (- stack-pointer 1) #f)
			(stack-set! stack-pointer (cadr act)))
		    (begin
		      (consume)
		      (skip))))))))

	(let loop ()
	  (if input
	      (let* ((state (stack-ref stack-pointer))
		     (i     (category-ref input))
		     (attr  (value-ref input))
		     (act   (action i (action-ref state))))
		(cond ((not (symbol? i))
		       (error-handler "syntax error: invalid token: " input)
		       #f)

		      ;;Input  succesfully parsed.   This happens  also when
		      ;;"*eoi*" is  found, which  means that the  value from
		      ;;the  vector is  returned to  the caller;  this value
		      ;;should be #<unspecified>.
		      ((eq? act 'accept)
		       (stack-ref 1))

		      ;;Syntax error in input.
		      ((eq? act '*error*)
		       (if (eq? i '*eoi*)
			   (begin
			     (error-handler "syntax error: unexpected end of input")
			     #f)
			 (begin
			   (error-handler "syntax error: unexpected token: " input)
			   (recover i)
			   (if (>= stack-pointer 0)
			       (set! input #f)
			     (begin
			       (set! stack-pointer 0)
			       (set! input '*eoi*)))
			   (loop))))

		      ;;Shift current token on top of the stack.
		      ((>= act 0)
		       (stack-push! attr act)
		       (set! input (if (eq? i '*eoi*) '*eoi* #f))
		       (loop))

		      ;;Reduce by rule (- act).
		      (else
		       (reduce (- act))
		       (loop))))

	    ;;No lookahead,  so check  if there is  a default  action that
	    ;;does not require the lookahead.
	    (let* ((state  (stack-ref  stack-pointer))
		   (acts   (action-ref state))
		   (defact (if (pair? acts) (cadar acts) #f)))
	      (if (and (= 1 (length acts))
		       (< defact 0))
		  (reduce (- defact))
		(consume))
	      (loop))))))))


;;;; done

)

;;; end of file

;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: packrat parser library
;;;Date: Sun Sep  6, 2009
;;;
;;;Abstract
;;;
;;;	The original code is distrubuted at:
;;;
;;;		<http://dev.lshift.net/tonyg/json-scheme/>
;;;
;;;	and it works with MzScheme.
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;;;Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining
;;;a  copy of  this  software and  associated  documentation files  (the
;;;"Software"), to  deal in the Software  without restriction, including
;;;without limitation  the rights to use, copy,  modify, merge, publish,
;;;distribute, sublicense,  and/or sell copies  of the Software,  and to
;;;permit persons to whom the Software is furnished to do so, subject to
;;;the following conditions:
;;;
;;;The  above  copyright notice  and  this  permission  notice shall  be
;;;included in all copies or substantial portions of the Software.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT. IN  NO EVENT SHALL THE AUTHORS  OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.
;;;


#!r6rs
(library (packrat)
  (export

    <state>   <state>?
    <state>-lookahead
    <state>-next-state
    <state>-memoized-results

    memoize-acons memoize-assq memoize-set!

    <result>                 <result>?  <result>-state
    <error>   make-<error>   <error>?   <error>-message
    <success> make-<success> <success>? <success>-value

    initialise-state next-state pushback

    make-terminal-combinator	make-sequence-combinator
    make-or-combinator		make-unless-combinator
    make-memoize-combinator	make-error-combinator
    make-grammar-combinator)
  (import (rnrs)
    (rnrs mutable-pairs)
    (language-extensions)
    (parser-tools lexical-token)
    (sentinel))


(define-record-type <result>
  (nongenerative nausicaa:packrat:<result>)
  (fields (immutable state)))

(define-record-type <error>
  (nongenerative nausicaa:packrat:<error>)
  (parent <result>)
  (fields (immutable message)))

(define-record-type <success>
  (nongenerative nausicaa:packrat:<success>)
  (parent <result>)
  (fields (immutable value)))

(define-condition-type &left-recursion
  &warning make-left-recursion-condition left-recursion-condition?
  (keyword condition-left-recursion))


(define-record-type <state>
  (fields (immutable lookahead)
	  (mutable   next-state)
	  (mutable   memoized-results)))

(define (initialise-state lexer)
  (define (state-maker)
    (make-<state> (lexer) state-maker '()))
  (state-maker))

(define (next-state state)
  (let ((state/state-maker (<state>-next-state state)))
    (if (<state>? state/state-maker)
	state/state-maker
      (let ((new-state (state/state-maker)))
	(<state>-next-state-set! state new-state)
	new-state))))

(define (pushback token state)
  (make-<state> token state '()))

(define-syntax memoize-acons
  (syntax-rules ()
    ((_ ?keyword ?result ?state)
     (<state>-memoized-results-set! ?state
				    (cons (cons ?keyword ?result)
					  (<state>-memoized-results ?state))))))

(define-syntax memoize-assq
  (syntax-rules ()
    ((_ ?keyword ?state)
     (assq ?keyword (<state>-memoized-results ?state)))))

(define-syntax memoize-set!
  (syntax-rules ()
    ((_ ?keyword ?result ?state)
     (set-cdr! (memoize-assq ?keyword ?state) ?result))))


(define (make-terminal-combinator terminal-category select-next-combinator)
  (lambda (state)
    (let ((token (<state>-lookahead state)))
      (if (eq? terminal-category (<lexical-token>-category token))
	  ((select-next-combinator (<lexical-token>-value token)) (next-state state))
	(make-<error> state (string-append "expected token with category "
					   (symbol->string terminal-category)))))))

(define (make-sequence-combinator combinator select-next-combinator)
  (lambda (state)
    (let ((result (combinator state)))
      (if (<success>? result)
	  ((select-next-combinator (<success>-value result)) (<result>-state result))
	result))))

(define (make-unless-combinator combinator-1 combinator-2)
  (lambda (state)
    (let ((result (combinator-1 state)))
      (if (<success>? result)
	  (make-<error> state "failed negation rule")
	(combinator-2 state)))))

(define make-memoize-combinator
  (case-lambda
   ((combinator)
    (make-memoize-combinator combinator (make-sentinel)))
   ((combinator keyword)
    (lambda (state)
      (let ((results-alist (<state>-memoized-results state)))
	(cond
	 ((assq keyword results-alist)
	  => (lambda (entry)
	       (let ((result (cdr entry)))
		 (or result
		     (raise-continuable
		      (condition (make-message-condition "left recursive combinator")
				 (make-irritants-condition (list keyword))
				 (make-left-recursion-condition keyword)))))))
	 (else
	  (let ((entry (cons keyword #f)))
	    (<state>-memoized-results-set! state (cons entry results-alist))
	    (begin0-let ((result (combinator state)))
	      (set-cdr! entry result))))))))))

(define (make-or-combinator comb-1 comb-2)
  (lambda (state)
    (let ((result (comb-1 state)))
      (if (<success>? result)
	  result
	(comb-2 state)))))

(define (make-error-combinator error-message)
  (lambda (state)
    (make-<error> state error-message)))


(define-syntax make-grammar-combinator
  ;;Define  a combinator  using a  grammar definition.   This  syntax is
  ;;explained in the documentation.
  ;;
  ;;The call to MATCH-RHS-RULES expands to a form which, when evaluated,
  ;;returns a combinator's function; the form:
  ;;
  ;;	((match-rhs-rules ---) state)
  ;;
  ;;is a call to the generated combinator and returns a <result>.  It is
  ;;wrapped into  a LAMBDA to  make LETREC* happy  about non-referencing
  ;;undefined  bindings:  this is  required  because  some expansion  of
  ;;MATCH-RHS-RULES is  a call to  MAKE-SEQUENCE-COMBINATOR, which takes
  ;;the value bound to a ?NONTERMINAL identifier as argument.
  ;;
  (syntax-rules ()
    ((_ ?start (?nonterminal (?sequence ?form ...) ...) ...)
     (letrec* ((?nonterminal (make-memoize-combinator
			      (lambda (state)
				((match-rhs-rules ((begin #f ?form ...) ?sequence)
						  ...)
				 state))))
	       ...)
       ?start))))

(define-syntax match-rhs-rules
  ;;Expand to a combinator's LAMBDA form or to a function call returning
  ;;a combinator's function.  Split the  first right-hand side rule in a
  ;;non-terminal   definition   from  the   others   and   pass  it   to
  ;;MATCH-SEQUENCE.
  ;;
  (syntax-rules ()
    ((_ (?semantic-clause ?sequence))
     (match-part ?semantic-clause ?sequence))

    ((_ (?semantic-clause ?sequence) ?rhs-rule ...)
     (make-or-combinator (match-part ?semantic-clause ?sequence)
			 (match-rhs-rules ?rhs-rule ...)))))

(define-syntax match-part
  ;;Expand to a combinator's LAMBDA form or to a function call returning
  ;;a  combinator's function.   Process the  parts  of a  sequence in  a
  ;;right-hand side rule  and convert them to a  combinator.  Every rule
  ;;below matches a single part, then recurses to match the others.
  ;;
  (syntax-rules (<- :not :error :exception :source-location)

    ((_ ?semantic-clause ())
     (lambda (state)
       (make-<success> state ?semantic-clause)))

    ((_ ?semantic-clause (:error ?error-message))
     (make-error-combinator ?error-message))

    ((_ ?semantic-clause (:exception))
     (lambda (state)
       (raise-continuable ?semantic-clause)))

    ((_ ?semantic-clause ((:not ?fail-part ...) ?part ...))
     (make-unless-combinator (match-part #t               (?fail-part ...))
			     (match-part ?semantic-clause (?part  ...))))

    ((_ ?semantic-clause (?var <- (quote ?terminal) ?part ...))
     (make-terminal-combinator (quote ?terminal)
			       (lambda (?var)
				 (match-part ?semantic-clause (?part ...)))))

    ((_ ?semantic-clause (?var <- :source-location ?part ...))
     (lambda (state)
       (let ((?var (<lexical-token>-location (<state>-lookahead state))))
	 ((match-part ?semantic-clause (?part ...)) state))))

    ((_ ?semantic-clause (?var <- ?nonterminal-identifier ?part ...))
     (make-sequence-combinator ?nonterminal-identifier
			       (lambda (?var)
				 (match-part ?semantic-clause (?part ...)))))

    ((_ ?semantic-clause ((quote ?terminal-symbol) ?part ...))
     (make-terminal-combinator (quote ?terminal-symbol)
			       (lambda (dummy)
				 (match-part ?semantic-clause (?part ...)))))

    ((_ ?semantic-clause (?nonterminal-identifier ?part ...))
     (make-sequence-combinator-identifier (lambda (dummy)
					    (match-part ?semantic-clause (?part ...)))))))


;;;; done

)

;;; end of file

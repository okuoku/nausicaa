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
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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

    <result>                 <result>?  <result>-state
    <error>   make-<error>   <error>?   <error>-message
    <success> make-<success> <success>? <success>-semantic-value

    initialise-state next-state
    pushback memoize-result

    make-terminal-combinator	make-sequence-combinator
    make-or-combinator		make-unless-combinator
    make-memoize-combinator	make-error-combinator
    make-grammar-combinator)
  (import (rnrs)
    (rnrs mutable-pairs)
    (language-extensions)
    (parser-tools lexical-token))


;;; helpers

(define (object->external-representation obj)
  (call-with-string-output-port
      (lambda (port)
	(write obj port))))


(define-record-type <result>
  (fields (immutable state)))

(define-record-type <error>
  (parent <result>)
  (fields (immutable message)))

(define-record-type <success>
  (parent <result>)
  (fields (immutable semantic-value)))


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

(define (memoize-result nonterminal-category result state)
  (<state>-memoized-results-set! (cons (cons nonterminal-category result)
				       (<state>-memoized-results state))))

(define (parse-nonterminal state nonterminal-category result-computer)
  (let ((results-alist (<state>-memoized-results state)))
    (cond
     ((assq nonterminal-category results-alist)
      => (lambda (entry)
      	   (let ((result (cdr entry)))
      	     (or result
      		 (assertion-violation 'parse-nonterminal
      		   "recursive non-terminal parse rule" nonterminal-category)))))
     (else
      (let ((entry (cons nonterminal-category #f)))
	(<state>-memoized-results-set! state (cons entry results-alist))
	(begin0-let ((result (result-computer)))
	  (set-cdr! entry result)))))))


;;;; middle-level combinator makers

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
	  ((select-next-combinator (<success>-semantic-value result)) (<result>-state result))
	result))))

(define (make-or-combinator combinator-1 combinator-2)
  (lambda (state)
    (let ((result-1 (combinator-1 state)))
      (if (<success>? result-1)
	  result-1
	(combinator-2 state)))))

(define (make-unless-combinator combinator-1 combinator-2)
  (lambda (state)
    (let ((result (combinator-1 state)))
      (if (<success>? result)
	  (make-<error> state "failed negation rule")
	(combinator-2 state)))))

(define (make-memoize-combinator nonterminal-category combinator)
  (lambda (state)
    (let ((results-alist (<state>-memoized-results state)))
      (cond
       ((assq nonterminal-category results-alist)
	=> (lambda (entry)
	     (let ((result (cdr entry)))
	       (or result
		   (assertion-violation 'parse-nonterminal
		     "recursive non-terminal parse rule" nonterminal-category)))))
       (else
	(let ((entry (cons nonterminal-category #f)))
	  (<state>-memoized-results-set! state (cons entry results-alist))
	  (begin0-let ((result (combinator state)))
	    (set-cdr! entry result))))))))

(define (make-error-combinator error-message)
  (lambda (state)
    (make-<error> state error-message)))


(define-syntax make-grammar-combinator
  ;;Define  a combinator  using a  grammar definition.   This  syntax is
  ;;explained in the documentation.
  ;;
  (syntax-rules ()
    ((_ ?start (?nonterminal (?sequence ?form ...) ...) ...)
     (letrec ()
       (define (?nonterminal state)
	 (parse-nonterminal state (quote ?nonterminal)
			    (lambda ()
			      ((match-rhs-rule ?nonterminal
					       ((begin #f ?form ...) ?sequence)
					       ...)
			       state))))
       ...
       ?start))))

(define-syntax match-rhs-rule
  ;;Split the  first right-hand side  rule in a  non-terminal definition
  ;;from the others and pass it to MATCH-SEQUENCE.  Used also to process
  ;;alternatives introduced by the ":or" grammar keyword.
  ;;
  (syntax-rules ()

    ((_ ?nonterminal (?semantic-clause ?sequence))
     (match-sequence ?nonterminal ?semantic-clause ?sequence))

    ((_ ?nonterminal (?semantic-clause ?sequence) ?rhs-rule0 ?rhs-rule ...)
     (make-or-combinator (match-sequence ?nonterminal ?semantic-clause ?sequence)
			 (match-rhs-rule ?nonterminal ?rhs-rule0 ?rhs-rule ...)))))

(define-syntax match-sequence
  ;;Process  the parts  of  a sequence  in  a right-hand  side rule  and
  ;;convert them  to a  combinator.  Every rule  below matches  a single
  ;;part, then recurses to match the others.
  ;;
  (syntax-rules (<- :not :error :exception :source-location)

    ;;Empty sequence, evaluate the  semantic clause and return a success
    ;;result.
    ((_ ?nonterminal ?semantic-clause ())
     (lambda (state)
       (make-<success> state ?semantic-clause)))

    ((_ ?nonterminal ?semantic-clause (:error ?error-message))
     (make-error-combinator ?error-message))

    ((_ ?nonterminal ?semantic-clause (:exception))
     (lambda (state)
       (raise ?semantic-clause)))

    ;;The first part is a sequence negation.
    ((_ ?nonterminal ?semantic-clause ((:not ?fail-part ...) ?rhs-part ...))
     (make-unless-combinator (match-sequence ?nonterminal #t               (?fail-part ...))
			     (match-sequence ?nonterminal ?semantic-clause (?rhs-part  ...))))

    ;;The first part binds a quoted value.
    ((_ ?nonterminal ?semantic-clause (?var <- (quote ?terminal) ?rhs-part ...))
     (make-terminal-combinator (quote ?terminal)
			       (lambda (?var)
				 (match-sequence ?nonterminal ?semantic-clause (?rhs-part ...)))))

    ;;The first part binds the current source location.
    ((_ ?nonterminal ?semantic-clause (?var <- :source-location ?rhs-part ...))
     (lambda (state)
       (let ((?var (<lexical-token>-location (<state>-lookahead state))))
	 ((match-sequence ?nonterminal ?semantic-clause (?rhs-part ...)) state))))

    ;;The first part matches  the lookahead token against a non-terminal
    ;;combinator and binds the result.
    ((_ ?nonterminal ?semantic-clause (?var <- ?nonterminal-identifier ?rhs-part ...))
     (make-sequence-combinator ?nonterminal-identifier
			       (lambda (?var)
				 (match-sequence ?nonterminal ?semantic-clause (?rhs-part ...)))))

    ;;The  first part  matches the  lookahead token  against  a terminal
    ;;symbol.
    ((_ ?nonterminal ?semantic-clause ((quote ?terminal-symbol) ?rhs-part ...))
     (make-terminal-combinator (quote ?terminal-symbol)
			       (lambda (dummy)
				 (match-sequence ?nonterminal ?semantic-clause (?rhs-part ...)))))

    ;;The first part matches  the lookahead token against a non-terminal
    ;;combinator.
    ((_ ?nonterminal ?semantic-clause (?nonterminal-identifier ?rhs-part ...))
     (make-sequence-combinator ?nonterminal-identifier
			       (lambda (dummy)
				 (match-sequence ?nonterminal ?semantic-clause (?rhs-part ...)))))))


;;;; done

)

;;; end of file

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
;;;Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;;;Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;;;
;;;Port to R6RS and Nausicaa integration by Marco Maggi.
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

    ;; parsing error
    <parse-error>		<parse-error>?
    <parse-error>-location	<parse-error>-expected
    <parse-error>-messages	<parse-error>-empty?
    make-<parse-error>		make-<parse-error>/expected
    make-<parse-error>/message
    merge-parse-errors

    ;; parse result
    <parse-result>		<parse-result>?
    <parse-result>-successful?	<parse-result>-semantic-value
    <parse-result>-next		<parse-result>-error
    make-<parse-result>
    make-<parse-result>/success	make-<parse-result>/expected
    make-<parse-result>/message	make-<parse-result>/merge-errors

    ;; parser state
    <parser-state>		<parser-state>?
    <parser-state>-location	<parser-state>-base
    <parser-state>-next-state

    ;; driver functions
    initialise-parser-state	compute-rule-result

    ;; miscellaneous
    pushback-token		prepend-precomputed-parse-result

    ;; middle level combinator makers
    packrat-check-base
    packrat-check
    packrat-or
    packrat-unless

    ;; high level combinator makers
    packrat-parser (rename (packrat-parser make-packrat-parser)))
  (import (rnrs)
    (rnrs mutable-pairs)
    (lists)
    (parser-tools lexical-token)
    (parser-tools source-location))


;;; helpers

(define gensym
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (string->symbol (string-append "gensym-" (number->string counter))))))

(define (%lexical-token-category/or-false token)
  ;;Return the token category, or #f  if TOKEN is #f or the end-of-input
  ;;marker.
  ;;
  (and token
       (let ((category (<lexical-token>-category token)))
	 (and (not (eq? '*eoi* category)) category))))

(define (object->external-representation obj)
  (call-with-string-output-port
      (lambda (port)
	(write obj port))))


(define-record-type <parse-error>
  (fields (immutable location)
	  (immutable expected)
	  (immutable messages)))

(define (make-<parse-error>/expected pos expected-terminal-category)
  (make-<parse-error> pos (list expected-terminal-category) '()))

(define (make-<parse-error>/message pos error-message)
  (make-<parse-error> pos '() (list error-message)))

(define (<parse-error>-empty? e)
  (and (null? (<parse-error>-expected e))
       (null? (<parse-error>-messages e))))

(define (merge-parse-errors e1 e2)
  ;;Merge   two   <parse-error>  records   and   return  the   resulting
  ;;<parse-error> record, which can be a  new record or one among E1 and
  ;;E2.
  ;;
  (cond ((not e1) e2)
	((not e2) e1)
	(else
	 (let ((p1 (<parse-error>-location e1))
	       (p2 (<parse-error>-location e2)))
	   (cond ((or (source-location-point>? p1 p2)
		      (<parse-error>-empty? e2))
		  e1)
		 ((or (source-location-point>? p2 p1)
		      (<parse-error>-empty? e1))
		  e2)
		 (else
		  (make-<parse-error> p1
				      (lset-union equal?
						  (<parse-error>-expected e1)
						  (<parse-error>-expected e2))
				      (lset-union equal?
						  (<parse-error>-messages e1)
						  (<parse-error>-messages e2)))))))))


(define-record-type <parse-result>
  (fields (immutable successful?)
	  (immutable semantic-value)
	  (immutable next)
	  (immutable error)))

(define (make-<parse-result>/success semantic-value next)
  (make-<parse-result> #t semantic-value next #f))

(define (make-<parse-result>/expected position expected-terminal-category)
  (%parse-error->parse-result (make-<parse-error>/expected position expected-terminal-category)))

(define (make-<parse-result>/message position error-message-string)
  (%parse-error->parse-result (make-<parse-error>/message position error-message-string)))

(define (%parse-error->parse-result parse-error)
  (make-<parse-result> #f #f #f parse-error))

(define (make-<parse-result>/merge-errors result error)
  (make-<parse-result> (<parse-result>-successful?    result)
		       (<parse-result>-semantic-value result)
		       (<parse-result>-next           result)
		       (merge-parse-errors (<parse-result>-error result) error)))


(define-record-type <parser-state>
  (fields (immutable location)
	  (immutable base)
	  (mutable next)
	  (mutable parse-result-map)))

(define (make-<parser-state>/success source-location base next-generator)
  (make-<parser-state> source-location base next-generator '()))

(define (make-<parser-state>/end-of-input source-location)
  (make-<parser-state> source-location #f #f '()))

(define (<parser-state>-next-state results)
  (let ((next (<parser-state>-next results)))
    (if (procedure? next)
	(let ((next-value (next)))
	  (<parser-state>-next-set! results next-value)
	  next-value)
      next)))

(define (%parser-state-token-value results)
  (let ((token (<parser-state>-base results)))
    (and (not (<lexical-token>?/end-of-input token))
	 (<lexical-token>-value token))))


(define (initialise-parser-state lexer)
  (define (results-generator)
    (let* ((token	(lexer))
	   (location	(<lexical-token>-location token)))
      (if (<lexical-token>?/end-of-input token)
	  (make-<parser-state>/end-of-input location)
	(make-<parser-state>/success location token results-generator))))
  (results-generator))

(define (compute-rule-result results nonterminal-category result-thunk)
  (let ((results-map (<parser-state>-parse-result-map results)))
    (cond
     ((assq nonterminal-category results-map)
      => (lambda (entry)
	   (let ((parse-result (cdr entry)))
	     (or parse-result
		 (error #f "recursive parse rule" nonterminal-category)))))
     (else
      (let ((entry (cons nonterminal-category #f)))
	(<parser-state>-parse-result-map-set! results (cons entry results-map))
	(let ((result (result-thunk)))
	  (set-cdr! entry result)
	  result))))))

(define (pushback-token source-location lexical-token state)
  (make-<parser-state> source-location lexical-token state '()))

(define (prepend-precomputed-parse-result source-location nonterminal-category
					  semantic-value state)
  (make-<parser-state> source-location #f #f
		       `(,(cons nonterminal-category
				(make-<parse-result>/success semantic-value state)))))


;;;; middle-level combinator makers

(define (packrat-check-base terminal-category k)
  (lambda (state)
    (let ((token (<parser-state>-base state)))
      (if (eq? (%lexical-token-category/or-false token) terminal-category)
	  ((k (<lexical-token>-value token)) (<parser-state>-next-state state))
	(make-<parse-result>/expected (<parser-state>-location state)
				      (if (not terminal-category)
					  '*eoi*
					terminal-category))))))

(define (packrat-check combinator k)
  (lambda (state)
    (let ((result (combinator state)))
      (if (<parse-result>-successful? result)
	  (make-<parse-result>/merge-errors ((k (<parse-result>-semantic-value result))
					     (<parse-result>-next result))
					    (<parse-result>-error result))
	result))))

(define (packrat-or combinator-1 combinator-2)
  (lambda (state)
    (let ((result (combinator-1 state)))
      (if (<parse-result>-successful? result)
	  result
	(make-<parse-result>/merge-errors (combinator-2 state)
					  (<parse-result>-error result))))))

(define (packrat-unless error-message combinator-1 combinator-2)
  (lambda (state)
    (let ((result (combinator-1 state)))
      (if (<parse-result>-successful? result)
	  (make-<parse-result>/message (<parser-state>-location state)
				       error-message)
	(combinator-2 state)))))


(define-syntax packrat-parser
  ;;Define a high-level combinator using a terminal/non-terminal grammar
  ;;definition.
  ;;
  ;;The subpatterns:  #f "alt", #f  "alts", are used to  distinguish the
  ;;internal  rewriting rules  from  the first  one,  which matches  the
  ;;client invocation of this macro.
  ;;
  (syntax-rules (<- ! :@ /)

    ;;Matches  the  client macro  use.   Process  all the  nonterminals,
    ;;converting each into a combinator's procedure.
    ((_ ?start (?nonterminal (?sequence form0 form ...) ...) ...)
     (let ()
       (define (?nonterminal state)
	 (compute-rule-result state (quote ?nonterminal)
			      (lambda ()
				((packrat-parser #f "alts" ?nonterminal
						 ((begin form0 form ...) ;the semantic clause
						  ?sequence)
						 ...)
				 state))))
       ...
       ?start))

    ;;All the rules below match while processing a single non-terminal's
    ;;right-hand side rules.

    ;;Matches the last right-hand side rule.
    ((_ #f "alts" ?nonterminal (?semantic-clause ?sequence))
     (packrat-parser #f "alt" ?nonterminal ?semantic-clause ?sequence))

    ;;Matches a right-hand side rule, not the last; split the first rule
    ;;from the other ones.
    ((_ #f "alts" ?nonterminal (?semantic-clause ?sequence) ?rhs-rule0 ?rhs-rule ...)
     (packrat-or (packrat-parser #f "alt"  ?nonterminal ?semantic-clause ?sequence)
		 (packrat-parser #f "alts" ?nonterminal ?rhs-rule0 ?rhs-rule ...)))

    ;;Matches a  single right-hand  side rule: Empty  part, so  return a
    ;;success result.
    ((_ #f "alt" ?nonterminal ?semantic-clause ())
     (lambda (state)
       (make-<parse-result>/success ?semantic-clause state)))

    ;;Matches a  single right-hand side  rule: The combinator  negates a
    ;;sequence.
    ((_ #f "alt" ?nonterminal ?semantic-clause ((! ?fails ...) ?rhs-part ...))
     (packrat-unless (string-append "non-terminal "
				    (symbol->string (quote ?nonterminal))
				    " expected to fail "
				    (object->external-representation '(?fails ...)))
		     (packrat-parser #f "alt" ?nonterminal #t               (?fails ...))
		     (packrat-parser #f "alt" ?nonterminal ?semantic-clause (?rhs-part  ...))))

    ;;Matches a  single right-hand side rule:  The combinator introduces
    ;;subordinate alternatives.
    ((_ #f "alt" ?nonterminal ?semantic-clause ((/ ?sequence ...) ?rhs-part ...))
     (packrat-check (packrat-parser #f "alts" ?nonterminal (#t ?sequence) ...)
		    (lambda (result)
		      (packrat-parser #f "alt" ?nonterminal ?semantic-clause (?rhs-part ...)))))

    ;;Matches  a single  right-hand side  rule: The  combinator  binds a
    ;;quoted value to a variable.
    ((_ #f "alt" ?nonterminal ?semantic-clause (?var <- (quote ?val) ?rhs-part ...))
     (packrat-check-base (quote ?val)
			 (lambda (?var)
			   (packrat-parser #f "alt" ?nonterminal ?semantic-clause (?rhs-part ...)))))

    ;;Matches  a single right-hand  side rule:  The combinato  binds the
    ;;current input stream location.
    ((_ #f "alt" ?nonterminal ?semantic-clause (?var <- :@ ?rhs-part ...))
     (lambda (state)
       (let ((?var (<parser-state>-location state)))
	 ((packrat-parser #f "alt" ?nonterminal ?semantic-clause (?rhs-part ...)) state))))

    ;;Matches a single right-hand  side rule: The combinator matches the
    ;;next token against a non-terminal combinator.
    ((_ #f "alt" ?nonterminal ?semantic-clause (?var <- ?nonterminal-identifier ?rhs-part ...))
     (packrat-check ?nonterminal-identifier
		    (lambda (?var)
		      (packrat-parser #f "alt" ?nonterminal ?semantic-clause (?rhs-part ...)))))

    ;;Matches a single right-hand  side rule: The combinator matches the
    ;;next token against a terminal symbol.
    ((_ #f "alt" ?nonterminal ?semantic-clause ((quote ?terminal-symbol) ?rhs-part ...))
     (packrat-check-base (quote ?terminal-symbol)
			 (lambda (dummy)
			   (packrat-parser #f "alt" ?nonterminal ?semantic-clause (?rhs-part ...)))))

    ;;Matches a single right-hand  side rule: The combinator matches the
    ;;next token against a non-terminal combinator.
    ((_ #f "alt" ?nonterminal ?semantic-clause (?nonterminal-identifier ?rhs-part ...))
     (packrat-check ?nonterminal-identifier
		    (lambda (dummy)
		      (packrat-parser #f "alt" ?nonterminal ?semantic-clause (?rhs-part ...)))))))


;;;; done

)

;;; end of file

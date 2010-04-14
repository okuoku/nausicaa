;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: portable higienic pattern matcher
;;;Date: Sat Aug 29, 2009
;;;
;;;Abstract
;;;
;;;	The original  code was written by  Alex Shinn and  placed in the
;;;	Public Domain.
;;;
;;;	Notice  that   some  SYNTAX-CASE  uses   have  "complex"  fender
;;;	expressions; they  could be put into a  separate library, loaded
;;;	for EXPAND, instead  they are kept here.  This  is to reduce the
;;;	overall  number of libraries  and to  improve "locality"  in the
;;;	reading comprehension of the code.
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;Copyright (c) 2006, 2007 Alex Shinn
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
(library (matches)
  (export
    match match-lambda match-lambda* match-define match-define*
    match-let match-letrec match-named-let match-let*

    &match-mismatch
    make-match-mismatch-condition
    match-mismatch-condition?
    condition-match-mismatch-expression
    match-mismatch-error)
  (import (rnrs)
    (rnrs mutable-pairs)
    (conditions))


(define-condition-type &match-mismatch
  &mismatch
  make-match-mismatch-condition
  match-mismatch-condition?
  (expr condition-match-mismatch-expression))

(define-syntax match-mismatch-error
  (syntax-rules ()
    ((_ ?who ?expr)
     (raise (condition (make-match-mismatch-condition ?expr)
		       (make-who-condition ?who)
		       (make-message-condition "no matching pattern"))))))


(define-syntax match
  ;;The basic interface.  Perform some basic syntax validation, bind the
  ;;expression to match to a temporary  variable (if it is not an atom),
  ;;and pass it on to NEXT-CLAUSE.
  ;;
  (lambda (stx)
    (syntax-case stx (=>)
      ((_)
       #'(syntax-violation 'match "missing match expression" '(match)))

      ((_ ?expr)
       #'(syntax-violation 'match "missing match clause" '(match ?expr)))

      ((_ ?expr (?pattern))
       #'(syntax-violation 'match "no body in match clause" '(?pattern)))

      ((_ ?expr (?pattern (=> ?failure)))
       #'(syntax-violation 'match "no body in match clause" '(?pattern (=> ?failure))))

      ((_ (?item ...) ?clause ...)
       #'(let ((expr (?item ...)))
	   (next-clause expr expr (set! expr) ?clause ...)))

      ((_ #(?item ...) ?clause ...)
       #'(let ((expr (quote #(?item ...))))
	   (next-clause expr expr (set! expr) ?clause ...)))

      ((_ ?atom ?clause ...)
       (identifier? #'?atom)
       #'(next-clause ?atom ?atom (set! ?atom) ?clause ...))

      ((_ ?atom ?clause ...)
       #'(next-clause ?atom ?atom (assertion-violation 'match "invoked setter for an atom") ?clause ...))

      )))


(define-syntax next-clause
  ;;Match  an expression  against the  full  pattern from  a clause;  if
  ;;matching  fails, invoke  a thunk  that  attempts to  match the  next
  ;;clause or  raise an error  if no other  clauses are present.   To be
  ;;called with the following arguments:
  ;;
  ;;EXPR	- the expression to match
  ;;GETTER	- the getter form accumulated for this clause
  ;;SETTER	- the setter form accumulated for this clause
  ;;CLAUSE ...	- one or more match clauses
  ;;
  ;;The EXPR value must be the name of a temporary variable to which the
  ;;expression to match  is bound, or the expression itself  if it is an
  ;;atom.  EXPR is meant to be evaluated multiple times in the expansion
  ;;of the macros.
  ;;
  ;;The GETTER  must be a form  which, when evaluated,  returns the full
  ;;expression;  the getter  can be  EXPR itself.   It is  used  only by
  ;;DISPATCH-PATTERN when the pattern has the form "(:getter <getter>)",
  ;;where <getter> is a symbol; the usage looks like this:
  ;;
  ;;	(let ((<getter> (lambda () GETTER)))
  ;;	  BODY)
  ;;
  ;;and the  thunk bound to  <getter> is supposed  to be invoked  in the
  ;;BODY.
  ;;
  ;;The  SETTER  must  be an  "incomplete"  form;  it  is used  only  by
  ;;DISPATCH-PATTERN when the pattern has the form "(:setter <setter>)",
  ;;where <setter> is a symbol; the usage looks like this:
  ;;
  ;;	(let ((<setter> (lambda (x)
  ;;	                  (?setter ... x))))
  ;;      . BODY)
  ;;
  ;;and  the the  accessor function  bound to  <setter> is  meant  to be
  ;;invoked by  the BODY.  The SETTER  form with X appended  is meant to
  ;;result to  a form  which, when  evaluated, sets the  value X  in the
  ;;expression.
  ;;
  (syntax-rules (=>)

    ;;No more clauses.
    ((_ ?expr ?accumulated-getter ?accumulated-setter)
     (match-mismatch-error 'match ?expr))

    ;;Match when the clause has an explicitly named match continuation.
    ((_ ?expr ?accumulated-getter ?accumulated-setter
	(?pattern (=> ?failure-kont) . ?body) . ?other-clauses)
     (let ((?failure-kont (lambda ()
			    (next-clause ?expr ?accumulated-getter ?accumulated-setter
					 . ?other-clauses))))
       (next-pattern ?expr ?pattern
		     ?accumulated-getter ?accumulated-setter
		     (match-drop-bound-pattern-variables
		      (begin . ?body)) ;success continuation
		     (?failure-kont) ;failure continuation
		     ()))) ;identifiers bound as pattern variables

    ;;Anonymous failure continuation, give it a dummy name and recurse.
    ((_ ?expr ?accumulated-getter ?accumulated-setter (?pattern . ?body) . ?other-clauses)
     (next-clause ?expr ?accumulated-getter ?accumulated-setter
		  (?pattern (=> anonymous-kont) . ?body) . ?other-clauses))))


(define-syntax next-pattern
  ;;Match an expression against a pattern element; it is a normalisation
  ;;step  before DISPATCH-PATTERN.   This macro  is the  entry  point to
  ;;match a pattern against an expression.
  ;;
  ;;To be called with the following arguments:
  ;;
  ;;EXPR		- the expression to match
  ;;PATTERN		- is the current pattern to match against EXPR
  ;;GETTER		- the getter form accumulated so far
  ;;SETTER		- the setter form accumulated so far
  ;;SUCCESS-KONT	- the success continuation
  ;;FAILURE-KONT	- the failure continuation
  ;;IDENTIFIERS		- the list of identifiers bound as pattern
  ;;			  variables so far
  ;;
  ;;See NEXT-CLAUSE for the meaning of EXPR, GETTER and SETTER.
  ;;
  ;;The  failure  continuation is  invoked  when  matching EXPR  against
  ;;PATTERN fails;  the continuation is  meant to invoke  NEXT-CLAUSE to
  ;;match  EXPR against  the next  clause.  The  FAILURE-KONT must  be a
  ;;thunk call, so that it is safe to expand it multiple times.
  ;;
  ;;If PATTERN is a list of two  or more values "(?p ?q . ?r)", check to
  ;;see  if   ?Q  is  an   ellipsis  and  handle  it   accordingly  with
  ;;ELLIPSIS-PATTERN; else pass all the arguments to DISPATCH-PATTERN.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ((_ ?expr (?pattern ?second-pattern . ?pattern-rest)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       (and (identifier? #'?second-pattern)
	    (free-identifier=? #'(... ...) #'?second-pattern))
       #'(extract-new-pattern-variables ?pattern
					(ellipsis-pattern ?expr ?pattern ?pattern-rest
							  ?accumulated-getter ?accumulated-setter
							  ?success-kont ?failure-kont
							  ?bound-pattern-variables)
					?bound-pattern-variables ()))

      ((_ ?expr (?pattern ?second-pattern . ?pattern-rest)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(dispatch-pattern ?expr (?pattern ?second-pattern . ?pattern-rest)
			   ?accumulated-getter ?accumulated-setter
			   ?success-kont ?failure-kont ?bound-pattern-variables))

      ((_ ?expr ?pattern
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(dispatch-pattern ?expr ?pattern
			   ?accumulated-getter ?accumulated-setter
			   ?success-kont ?failure-kont ?bound-pattern-variables))

      )))


(define-syntax match-drop-bound-pattern-variables
  ;;Occasionally NEXT-PATTERN is used  with a success continuation which
  ;;not  only does not  need the  list of  bound pattern  variables, but
  ;;which would raise an error if the list of variables is appended:
  ;;
  ;;	(next-pattern <expr> <pattern> <getter> <setter>
  ;;		(<success-kont> ...)
  ;;		<failure-kont> <bound-pattern-variables>)
  ;;
  ;;would expand the success continuation to:
  ;;
  ;;    (<success-kont> ... <bound-pattern-variables>) -> ERROR!!!
  ;;
  ;;In these  cases we can wrap  the continuation with  this macro which
  ;;has the only effect of dropping the list of variables:
  ;;
  ;;	(next-pattern <expr> <pattern> <getter> <setter>
  ;;		(match-drop-bound-pattern-variables
  ;;		   (<success-kont> ...))
  ;;		<failure-kont> <bound-pattern-variables>)
  ;;
  ;;expands the success continuation to:
  ;;
  ;;    (match-drop-bound-pattern-variables
  ;;       (<success-kont> ...)
  ;;       <bound-pattern-variables>)
  ;;
  ;;which then expands to:
  ;;
  ;;    (<success-kont> ...)
  ;;
  ;;This macro removes both the  list of already bound pattern variables
  ;;and    the   list    of   to-be-bound    pattern    variables;   see
  ;;EXTRACT-NEW-PATTERN-VARIABLES for details.
  ;;
  (syntax-rules ()

    ((_ (?success-kont ...) ?bound-pattern-variables)
     (?success-kont ...))

    ((_ (?success-kont ...) ?bound-pattern-variables ?to-be-bound-pattern-variables)
     (?success-kont ...))

    ((_ (?success-kont ...))
     (?success-kont ...))
    ))


(define-syntax dispatch-pattern
  ;;Match a  pattern element against all the  supported pattern matching
  ;;models; invoke  the appropriate macro.   To be called with  the same
  ;;arguments of NEXT-PATTERN.
  ;;
  (lambda (stx)
    (syntax-case stx (:predicate :accessor :and :or :not :setter :getter quasiquote quote)

      ;;the pattern is #t
      ((_ ?expr #t
      	  ?accumulated-getter ?accumulated-setter
      	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(if ?expr
      	     (?success-kont ... ?bound-pattern-variables)
      	   ?failure-kont))

      ;;the pattern is #f
      ((_ ?expr #f
      	  ?accumulated-getter ?accumulated-setter
      	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(if (not ?expr)
      	     (?success-kont ... ?bound-pattern-variables)
      	   ?failure-kont))

;;; --------------------------------------------------------------------

      ;;the pattern is a quoted identifier
      ((_ ?expr (quote ?pattern)
	  ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       (identifier? #'?pattern)
       #'(if (eq? ?expr (quote ?pattern))
	     (?success-kont ... ?bound-pattern-variables)
	   ?failure-kont))

      ;;the pattern is a quoted S-expression
      ((_ ?expr (quote ?pattern)
	  ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(if (equal? ?expr (quote ?pattern))
	     (?success-kont ... ?bound-pattern-variables)
	   ?failure-kont))

      ;;the pattern is a quasiquoted sexp
      ((_ ?expr (quasiquote ?pattern)
	  ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(let ((pattern (quasiquote ?pattern)))
	   (if (equal? ?expr pattern)
	       (?success-kont ... ?bound-pattern-variables)
	     ?failure-kont)))

;;; --------------------------------------------------------------------

      ;;the pattern is the empty :AND
      ((_ ?expr (:and)
	  ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(?success-kont ... ?bound-pattern-variables))

      ;;the pattern is a single-clause AND
      ((_ ?expr (:and ?pattern)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(next-pattern ?expr ?pattern
		       ?accumulated-getter ?accumulated-setter
		       ?success-kont ?failure-kont ?bound-pattern-variables))

      ;;the pattern is a non-empty AND
      ((_ ?expr (:and ?first-pattern ?other-pattern ...)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(next-pattern ?expr ?first-pattern
		       ?accumulated-getter ?accumulated-setter
		       ;;The  following  is  the  success  continuation.
		       ;;Notice    that    ?BOUND-PATTERN-VARIABLES   is
		       ;;appended to it by the nested NEXT-PATTERN use.
		       (next-pattern ?expr (:and ?other-pattern ...)
				     ?accumulated-getter ?accumulated-setter
				     ?success-kont ?failure-kont)
		       ?failure-kont ?bound-pattern-variables))

;;; --------------------------------------------------------------------

      ;;the pattern is an empty OR
      ((_ ?expr (:or)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'?failure-kont)

      ;;the pattern is a single-clause OR
      ((_ ?expr (:or ?pattern)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(next-pattern ?expr ?pattern
		       ?accumulated-getter ?accumulated-setter
		       ?success-kont ?failure-kont ?bound-pattern-variables))

      ;;the pattern is a multiple-clause OR
      ((_ ?expr (:or ?pattern ...)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(extract-new-pattern-variables (:or ?pattern ...)
					(generate-or ?expr (?pattern ...)
						     ?accumulated-getter ?accumulated-setter
						     ?success-kont ?failure-kont
						     ?bound-pattern-variables)
					?bound-pattern-variables ()))

;;; --------------------------------------------------------------------

      ;;the pattern is an empty :NOT form
      ((_ ?expr (:not)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(syntax-violation 'match "empty :NOT form in pattern" '(:not)))

      ;;the pattern is a :NOT form
      ((_ ?expr (:not ?pattern)
	  ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(next-pattern ?expr ?pattern
		       ?accumulated-getter ?accumulated-setter
		       (match-drop-bound-pattern-variables ?failure-kont)
		       (?success-kont ... ?bound-pattern-variables)
		       ?bound-pattern-variables))

;;; --------------------------------------------------------------------

      ;;the pattern is a getter
      ((_ ?expr (:getter getter)
	  ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(let ((getter (lambda () ?accumulated-getter)))
	   (?success-kont ... ?bound-pattern-variables)))

      ;;the pattern is a setter
      ((_ ?expr (:setter setter)
	  ?accumulated-getter (?accumulated-setter ...)
	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(let ((setter (lambda (x) (?accumulated-setter ... x))))
	   (?success-kont ... ?bound-pattern-variables)))

;;; --------------------------------------------------------------------

      ;;the pattern is a quasiquoted predicate
      ((_ ?expr (:predicate (quasiquote ?predicate) ?pattern ...)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(let ((predicate (quasiquote ?predicate)))
	   (if (predicate ?expr)
	       (next-pattern ?expr (:and ?pattern ...)
			     ?accumulated-getter ?accumulated-setter
			     ?success-kont ?failure-kont ?bound-pattern-variables)
	     ?failure-kont)))

      ;;the pattern is a predicate
      ((_ ?expr (:predicate ?predicate ?pattern ...)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(if (?predicate ?expr)
	     (next-pattern ?expr (:and ?pattern ...)
			   ?accumulated-getter ?accumulated-setter
			   ?success-kont ?failure-kont ?bound-pattern-variables)
	   ?failure-kont))

;;; --------------------------------------------------------------------

      ;;the pattern is an accessor matcher with quasiquoted procedure
      ((_ ?expr (:accessor (quasiquote ?proc) ?pattern)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(let ((proc (quasiquote ?proc)))
	   (let ((expr1 (proc ?expr)))
	     (next-pattern expr1 ?pattern
			   ?accumulated-getter ?accumulated-setter
			   ?success-kont ?failure-kont ?bound-pattern-variables))))

      ;;the pattern is an accessor matcher
      ((_ ?expr (:accessor ?proc ?pattern)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(let ((expr1 (?proc ?expr)))
	   (next-pattern expr1 ?pattern
			 ?accumulated-getter ?accumulated-setter
			 ?success-kont ?failure-kont ?bound-pattern-variables)))

;;; --------------------------------------------------------------------

      ;;the pattern is null
      ((_ ?expr ()
	  ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont ?bound-pattern-variables)
       #'(if (null? ?expr)
	     (?success-kont ... ?bound-pattern-variables)
	   ?failure-kont))

      ;;the pattern is a one-element list
      ((_ ?expr (?pattern)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(if (and (pair? ?expr)
		  (null? (cdr ?expr)))
	     (let-syntax ((sub-expr (identifier-syntax (car ?expr))))
	       (next-pattern sub-expr ?pattern
			     sub-expr (set-car! ?expr)
			     ?success-kont ?failure-kont ?bound-pattern-variables))
	   ?failure-kont))

      ;;the pattern is a pair
      ((_ ?expr (?pattern . ?pattern-rest)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(if (pair? ?expr)
	     (let-syntax ((expr-a (identifier-syntax (car ?expr)))
			  (expr-d (identifier-syntax (cdr ?expr))))
	       (next-pattern expr-a ?pattern
			     (car ?expr) (set-car! ?expr)
			     ;;The    following     is    the    success
			     ;;continuation.          Notice        that
			     ;;?BOUND-PATTERN-VARIABLES  is  appended to
			     ;;it by the nested NEXT-PATTERN use.
			     (next-pattern expr-d ?pattern-rest
					   (cdr ?expr) (set-cdr! ?expr)
					   ?success-kont ?failure-kont)
			     ?failure-kont
			     ?bound-pattern-variables))
	   ?failure-kont))

;;; --------------------------------------------------------------------

      ;;the pattern is a vector
      ((_ ?expr #(?pattern ...)
	  ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(match-vector ?expr
		       0  ;index of the first element
		       () ;list of index patterns
		       (?pattern ...)
		       ?success-kont ?failure-kont ?bound-pattern-variables))

;;; --------------------------------------------------------------------

      ;;the pattern is the wildcard identifier
      ((_ v underscore g s (sk ...) fk i)
       (and (identifier? #'underscore) (free-identifier=? #'_ #'underscore))
       #'(sk ... i))

      ;;the pattern is a literal or a pattern variable among the already
      ;;bound ones
      ((_ ?expr ?pattern ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont (?bound-pattern-variable ...))
       (or (not (identifier? #'?pattern))
	   (memq (syntax->datum #'?pattern) (syntax->datum #'(?bound-pattern-variable ...))))
       #'(if (equal? ?expr ?pattern)
	     (?success-kont ... (?bound-pattern-variable ...))
	   ?failure-kont))

      ;;the pattern is a pattern  variable NOT already bound, we bind it
      ;;and add it to the list of bound pattern variables
      ((_ ?expr ?pattern ?accumulated-getter ?accumulated-setter
	  (?success-kont ...) ?failure-kont (?bound-pattern-variable ...))
       (identifier? #'?pattern)
       #'(let ((?pattern ?expr))
	   (?success-kont ... (?bound-pattern-variable ... ?pattern))))

      ;;The  following is  the  original pattern-var/literal  processing
      ;;clause;  I am  keeping it  because  the syntax  trick is  really
      ;;spiffy.
      ;;
      ;; ((_ ?expr ?pattern g s (sk ...) fk (id ...))
      ;;  #'(let-syntax ((new-sym? (syntax-rules (id ...)
      ;; 		;If ?PATTERN is among the ID symbols listed as literals,
      ;; 		;the first rule matches; else the second rule matches.
      ;; 				  ((_ ?pattern sk2 fk2) sk2)
      ;; 				  ((_ y        sk2 fk2) fk2))))
      ;; 	   (new-sym? random-sym-to-match
      ;; 		     (let ((?pattern ?expr))
      ;; 		       (sk ... (id ... ?pattern)))
      ;; 		     (if (equal? ?expr ?pattern)
      ;; 			 (sk ... (id ...))
      ;; 		       fk))))

;;; --------------------------------------------------------------------

      ;;we should never reach this clause
      ((_ ?expr ?pattern ?accumulated-getter ?accumulated-setter
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(syntax-violation 'match "unrecognised pattern element" ?pattern))

      )))


(define-syntax generate-or
  ;;Generating OR clauses just involves binding the success continuation
  ;;into a  thunk which takes the  identifiers from each  OR clause, and
  ;;trying each clause, calling the thunk as soon as we succeed.
  ;;
  (syntax-rules ()
    ((_ v p g s (sk ...) fk (i ...) (id ...))
     (let ((sk2 (lambda (id ...) (sk ... (i ... id ...)))))
       (generate-or-step v p g s
			  (match-drop-bound-pattern-variables (sk2 id ...))
			  fk (i ...))))))

(define-syntax generate-or-step
  (syntax-rules ()

    ;;no OR clauses, call the failure continuation
    ((_ ?expr () ?getter ?setter ?success-kont ?failure-kont ?identifiers)
     ?failure-kont)

    ;;last (or only) OR clause, just expand normally
    ((_ ?expr (?pattern) ?getter ?setter ?success-kont ?failure-kont ?identifiers)
     (next-pattern ?expr ?pattern ?getter ?setter ?success-kont ?failure-kont ?identifiers))

    ;;match one and try the remaining on failure
    ((_ ?expr (?pattern . ?pattern-rest) ?getter ?setter ?success-kont ?failure-kont ?identifiers)
     (next-pattern ?expr ?pattern ?getter ?setter
		   ?success-kont
		   (generate-or-step ?expr ?pattern-rest ;failure continuation
				      ?getter ?setter
				      ?success-kont ?failure-kont ?identifiers)
		   ?identifiers))))


(define-syntax ellipsis-pattern
  ;;Match  the expression with  a list  pattern containing  an ellipsis;
  ;;this macro  is called by  NEXT-PATTERN when it has  already verified
  ;;that the expression ?EXPR is null or a pair.
  ;;
  ;;This  macro can  be called  with list  input pattern  in one  of the
  ;;following forms, in  which the ellipsis identifier is  always in the
  ;;cadr position:
  ;;
  ;;	(?identifier-pattern ?ellipsis-identifier)
  ;;	(?some-pattern       ?ellipsis-identifier)
  ;;	(?some-pattern       ?ellipsis-identifier . ?pattern-rest)
  ;;
  ;;* In the first form we just take the whole expression ?EXPR and bind
  ;;it to ?IDENTIFIER-PATTERN.
  ;;
  ;;* In the second form we  use a loop to match every sub-expression in
  ;;?EXPR against ?SOME-PATTERN, using NEXT-PATTERN.
  ;;
  ;;* In the third form we assume that ?PATTERN-REST is a list (possibly
  ;;null), so we determine its length and compute the number of elements
  ;;in ?EXPR to match against ?SOME-PATTERN:
  ;;
  ;;	(define number-of-subexpressions-to-match
  ;;	   (- (length ?EXPR) (length ?PATTERN-REST)))
  ;;
  ;;the match is  positive if ?EXPR has enough  sub-expressions to match
  ;;all the patterns in ?PATTERN-REST;  it may be that no sub-expression
  ;;is available to be matched by  ?SOME-PATTERN.  Then we use a loop to
  ;;match the sub-expressions agains ?SOME-PATTERN, using NEXT-PATTERN.
  ;;
  ;;Synopsis:
  ;;
  ;;   (ellipsis-pattern	?expr ?pattern ?pattern-rest
  ;;				?accumulated-getter ?accumulated-setter
  ;;				?success-kont ?failure-kont
  ;;				?already-bound-pattern-variables
  ;;				?to-be-bound-pattern-variables)
  ;;
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;Match  the  pattern  "(?pattern   ...)"   when  ?PATTERN  is  an
      ;;identifier: bind the expression to  it, whatever it is, and call
      ;;the continuation.
      ((_ ?expr ?pattern () ;pattern rest
	  ?getter ?setter
	  (?success-kont ...) ?failure-kont
	  ?bound-pattern-variables ?to-be-bound-pattern-variables)
       (identifier? #'?pattern)
       #'(let ((?pattern ?expr))
	   (?success-kont ... ?bound-pattern-variables)))

      ;;Match  the pattern  "(?pattern ...)"   when ?PATTERN  is  not an
      ;;identifier:  match  the   nested  patterns  against  the  nested
      ;;expressions.
      ((_ ?expr ?pattern () ;pattern rest
	  ?getter ?setter
	  (?success-kont ...) ?failure-kont
	  ?bound-pattern-variables
	  (to-be-bound-pattern-variable ...))
       (with-syntax (((pattern-variable-values ...)
		      (generate-temporaries #'(to-be-bound-pattern-variable ...))))
	 #'(let loop ((sub-expressions ?expr)
		      (pattern-variable-values '())
		      ...)
	     (cond ((null? sub-expressions)
		    (let ((to-be-bound-pattern-variable (reverse pattern-variable-values))
			  ...)
		      (?success-kont ... ?bound-pattern-variables)))
		   ((pair? sub-expressions)
		    (let ((sub-expr (car sub-expressions)))
		      (next-pattern sub-expr ?pattern
				    sub-expr (set-car! sub-expressions)
				    ;;The   following   is   the   success
				    ;;continuation.   Notice that  we have
				    ;;to drop  the bound pattern variables
				    ;;appended by NEXT-PATTERN.
				    (match-drop-bound-pattern-variables
				     (loop (cdr sub-expressions)
					   (cons to-be-bound-pattern-variable
						 pattern-variable-values) ...))
				    ?failure-kont ?bound-pattern-variables)))
		   (else
		    ?failure-kont)))))

      ;;Detect multiple ellipsis at the same level in the list pattern.
      ((_ ?expr ?pattern (?pattern-rest ...) getter ?setter
	  (?success-kont ...) ?failure-kont
	  ?bound-pattern-variables ?to-be-bound-pattern-variables)
       (memq '... (syntax->datum #'(?pattern-rest ...)))
       #'(syntax-violation 'match
	   "multiple ellipsis patterns not allowed at same level"
	   '(?pattern (... ...) . (?pattern-rest ...))))

      ;;Match  the  pattern  "(?pattern  ...  .   ?pattern-rest)"  where
      ;;?PATTERN-REST are trailing patterns.
      ((_ ?expr ?pattern (?pattern-rest ...)
	  ?getter ?setter
	  (?success-kont ...) ?failure-kont
	  ?bound-pattern-variables
	  (?to-be-bound-pattern-variable ...))
       (with-syntax (((pattern-variable-values ...)
		      (generate-temporaries #'(?to-be-bound-pattern-variable ...))))
	 #'(let* ((rest-len		(length '(?pattern-rest ...)))
		  (sub-expressions	?expr)
		  (len			(length sub-expressions)))
	     (if (< len rest-len)
		 ;;Not enough sub-expressions to match the rest patterns.
		 ?failure-kont
	       (let loop ((sub-expressions		sub-expressions)
			  (still-to-match-len		len)
			  (pattern-variable-values	'())
			  ...)
		 (cond

		  ;;When  the  number  of  sub-expressions left  to  match
		  ;;equals  the  number of  patterns  in  the rest:  start
		  ;;matching the rest.
		  ((= still-to-match-len rest-len)
		   (let ((?to-be-bound-pattern-variable (reverse pattern-variable-values))
			 ...)
		     (next-pattern sub-expressions (?pattern-rest ...) #f #f
				   (?success-kont ... ?bound-pattern-variables) ?failure-kont
				   ?bound-pattern-variables)))

		  ;;Match  the next  sub-expression against  the ellipsis'
		  ;;pattern.
		  ((pair? sub-expressions)
		   (next-pattern (car sub-expressions) ?pattern
				 (car sub-expressions) (set-car! sub-expressions)
				 ;;The    following    is   the    success
				 ;;continuation.   Notice that we  have to
				 ;;drop   the   bound  pattern   variables
				 ;;appended by NEXT-PATTERN.
				 (match-drop-bound-pattern-variables
				  (loop (cdr sub-expressions) (- still-to-match-len 1)
					(cons ?to-be-bound-pattern-variable
					      pattern-variable-values)
					...))
				 ?failure-kont
				 ?bound-pattern-variables))

		  (else
		   ?failure-kont)))))))
      )))


(define-syntax match-vector
  ;;This syntax is  the entry point for vector matching,  but it is also
  ;;called recursively.  Synopsis:
  ;;
  ;;	(match-vector ?expression-to-be-matches
  ;;		      0		;index of the first element
  ;;                  ()	;list of index-patterns
  ;;                  (?vector-pattern ...)
  ;;                  ?success-continuation ?failure-continuation
  ;;                  ?identifiers)
  ;;
  ;;The vector pattern (without ellipsis):
  ;;
  ;;	(match expr
  ;;	  (#(a b c) 'ok))
  ;;
  ;;is expanded to:
  ;;
  ;;	(match-vector expr
  ;;			0  ()  (a b c)
  ;;			<success-continuation>
  ;;			<failure-continuation> <identifiers>)
  ;;
  ;;where "0" is the index of the first element; then it is expanded to:
  ;;
  ;;	(match-vector expr
  ;;			3  ((a 0) (b 1) (c 2))  ()
  ;;			<success-continuation>
  ;;			<failure-continuation> <identifiers>)
  ;;
  ;;where "((a  0) (b  1) (c  1))" are the  patterns coupled  with their
  ;;indexes in the vector (these are called "index-patterns") and "3" is
  ;;the length of the vector.
  ;;
  ;;Matching   in  case   an   ellipsis  is   found   is  delegated   to
  ;;MATCH-VECTOR-ELLIPSIS such that:
  ;;
  ;;	(match expr
  ;;	  (#(a b c d ...) 'ok))
  ;;
  ;;is expanded to:
  ;;
  ;;	(match-vector-ellipsis expr
  ;;			3  ((a 0) (b 1) (c 2))  (d)
  ;;			<success-continuation>
  ;;			<failure-continuation> <identifiers>)
  ;;
  ;;where "3"  is the last index-pattern's  index plus 1 and  "d" is the
  ;;pattern which must match all the elements selected by the ellipsis.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;Matches   only   if  the   vector   pattern   is  "#(...)",   so
      ;;?SINGLE-PATTERN is the ellipsis.
      ((_ v ?next-index ?index-patterns (?single-pattern) sk fk i)
       (and (identifier? #'?single-pattern)
	    (free-identifier=? #'?single-pattern #'(... ...)))
       #'(syntax-violation 'match
	   "ellipsis not allowed as single, vector pattern value"
	   '#(?single-pattern)))

      ;;Detect if the last pattern in the vector is an ellipsis.
      ;;
      ;;*FIXME* Currently the ellipsis can appear only as the last element.
      ((_ v ?next-index ?index-patterns (?penultimate-pattern ?last-pattern) sk fk i)
       (and (identifier? #'?last-pattern)
	    (free-identifier=? #'?last-pattern #'(... ...)))
       #'(match-vector-ellipsis v ?next-index ?index-patterns ?penultimate-pattern sk fk i))

      ;;All the  patterns have been converted to  index-patterns.  Check
      ;;the exact vector length, then match each element in turn.
      ((_ ?expr ?vector-len ((?pattern ?index) ...) () sk fk i)
       #'(if (vector? ?expr)
	     (let-syntax ((len (identifier-syntax (vector-length ?expr))))
	       (if (= len ?vector-len)
		   (match-vector-index-pattern ?expr ((?pattern ?index) ...) sk fk i)
		 fk))
	   fk))

      ;;Convert the next pattern into an index-pattern.
      ((_ ?expr ?next-index (?index-pattern ...) (?pattern . ?pattern-rest) sk fk i)
       #'(match-vector ?expr (+ ?next-index 1)
		       (?index-pattern ... (?pattern ?next-index))
		       ?pattern-rest sk fk i))
      )))

(define-syntax match-vector-index-pattern
  ;;Expand to the  code needed to match the  next index-pattern.  Expand
  ;;recursively until all the index-patterns have been processed.
  ;;
  (syntax-rules ()

    ;;No more patterns, success.
    ((_ v () (sk ...) fk i)
     (sk ... i))

    ;;Match an element, then match recursively the next one.
    ((_ ?expr ((?pattern ?index) . ?rest) sk fk i)
     (let-syntax ((item (identifier-syntax (vector-ref ?expr ?index))))
       (next-pattern item ?pattern
		     (vector-ref ?expr ?index) (vector-set! ?expr ?index)
		     (match-vector-index-pattern ?expr ?rest sk fk) ;success continuation
		     fk i)))))

(define-syntax match-vector-ellipsis
  ;;With a  vector ellipsis pattern we  first check to see  if the vector
  ;;length is at least the required length.
  ;;
  (syntax-rules ()
    ;;The ?TAIL-PATTERN is the one that must match all the tail items in
    ;;the vector.
    ((_ ?expr ?number-of-index-patterns ((?pattern ?index) ...) ?ellipsis-pattern sk fk i)
     (if (vector? ?expr)
	 (let ((expr-vector-len (vector-length ?expr)))
	   (if (>= expr-vector-len ?number-of-index-patterns)
	       (match-vector-index-pattern ?expr ((?pattern ?index) ...)
					   (match-vector-tail ?expr expr-vector-len
							      ?ellipsis-pattern
							      ?number-of-index-patterns
							      sk fk)
					   fk i)
	     fk))
       fk))))

(define-syntax match-vector-tail
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?expr ?expr-vector-len ?ellipsis-pattern ?number-of-index-patterns
	  ?success-kont ?failure-kont ?bound-pattern-variables)
       #'(extract-new-pattern-variables
	  ?ellipsis-pattern
	  (match-vector-tail #t ?expr ?expr-vector-len
			     ?ellipsis-pattern
			     ?number-of-index-patterns
			     ?success-kont ?failure-kont ?bound-pattern-variables)
	  ?bound-pattern-variables ()))

      ((_ #t v ?expr-vector-len ?ellipsis-pattern ?number-of-index-patterns
	  (?success-kont ...) ?failure-kont
	  ?bound-pattern-variables (?to-be-bound-pattern-variable ...))
       (with-syntax (((pattern-variable-values ...)
		      (generate-temporaries #'(?to-be-bound-pattern-variable ...))))
	 ;;loop over the tail vector indices
	 #'(let loop ((j			?number-of-index-patterns)
		      (pattern-variable-values '())
		      ...)
	     (if (>= j ?expr-vector-len)
		 (let ((?to-be-bound-pattern-variable (reverse pattern-variable-values))
		       ...)
		   (?success-kont ... ?bound-pattern-variables))
	       (let-syntax ((sub-expression (identifier-syntax (vector-ref v j))))
		 (next-pattern sub-expression ?ellipsis-pattern
			       (vector-ref v j) (vetor-set! v j)
			       (match-drop-bound-pattern-variables
				(loop (+ j 1)
				      (cons ?to-be-bound-pattern-variable pattern-variable-values)
				      ...))
			       ?failure-kont ?bound-pattern-variables)))))))))


(define-syntax extract-new-pattern-variables
  ;;Extract  all  the  identifiers  in a  pattern  representing  pattern
  ;;variables.  Unfortunately all  the branches in DISPATCH-PATTERN must
  ;;be duplicated here.
  ;;
  ;;The need for this macro  arises from the feature of matching already
  ;;bound pattern variables with the value assigned before; example:
  ;;
  ;;	(match '(ok . ok)
  ;;	  ((X . X)
  ;;	   <body>))
  ;;
  ;;the  first occurrence  of  X in  the  pattern causes  a new  pattern
  ;;variable   to   be  bound,   the   second   occurrence  causes   the
  ;;sub-expression to be compared to the value already bound:
  ;;
  ;;	(let ((expr '(ok . ok)))
  ;;      (let ((X (car expr)))		;first time, bind
  ;;        (if (equal? X)		;second time, compare
  ;;            <body>
  ;;          (match-mismatch-error 'match (cdr expr)))))
  ;;
  ;;The simple cases  of pattern variables in pairs  or lists or vectors
  ;;without  ellipsis,  are  handled  directly by  DISPATCH-PATTERN  and
  ;;MATCH-VECTOR; this macro handles the  case of pattern followed by an
  ;;ellipsis, both  in lists and vectors,  and the case  of multiple :OR
  ;;clauses.   For this complex  cases it  is required  to know  all the
  ;;newly defined pattern variable identifiers while building the output
  ;;form.
  ;;
  ;;Synopsis:
  ;;
  ;;    (extract-new-pattern-variables
  ;;		<pattern> (<continuation> ...)
  ;;		(<already-bound-pattern-variable> ...)
  ;;		(<extracted-var> ...))
  ;;
  ;;The purpose of  this macro is to find new  pattern variables and add
  ;;them to  the <EXTRACTED-VAR> list; finally the  continuation is used
  ;;as output form with the variables appended:
  ;;
  ;;    (<continuation> ... (<extracted-var> ...))
  ;;
  ;;The  list of <ALREADY-BOUND-PATTERN-VARIABLE>  identifiers represent
  ;;the  pattern variables  which  where already  bound  before: when  a
  ;;variable is  found which is already  bound, it is  ignored; only new
  ;;pattern variables are registered by this macro.
  ;;
  ;;When called by  other macros: the list of  <EXTRACTED-VAR> is always
  ;;null; this  macro calls  itself recursively so  the list can  be not
  ;;null for a macro use.
  ;;
  (lambda (stx)
    (syntax-case stx (:predicate :accessor :and :or :not :getter :setter quasiquote quote)

      ;;The pattern is the literal #t.
      ((_ #t (?kont ...) ?bound-pattern-variables ?extracted-variables)
       #'(?kont ... ?extracted-variables))

      ;;The pattern is the literal #f.
      ((_ #f (?kont ...) ?bound-pattern-variables ?extracted-variables)
       #'(?kont ... ?extracted-variables))

      ;;The pattern is the :PREDICATE special keyword.
      ((_ (:predicate pred . ?rest-pattern) ?kont ?bound-pattern-variables ?extracted-variables)
       #'(extract-new-pattern-variables ?rest-pattern ?kont
					?bound-pattern-variables ?extracted-variables))

      ;;The pattern is the :ACCESSOR special keyword.
      ((_ (:accessor accessor ?pattern) ?kont ?bound-pattern-variables ?extracted-variables)
       #'(extract-new-pattern-variables ?pattern ?kont ?bound-pattern-variables ?extracted-variables))

      ;;The pattern is the :AND special keyword.
      ((_ (:and . ?and-patterns) ?kont ?bound-pattern-variables ?extracted-variables)
       #'(extract-new-pattern-variables ?and-patterns ?kont
					?bound-pattern-variables ?extracted-variables))

      ;;The pattern is the :OR special keyword.
      ((_ (:or . ?or-patterns) ?kont ?bound-pattern-variables ?extracted-variables)
       #'(extract-new-pattern-variables ?or-patterns ?kont
					?bound-pattern-variables ?extracted-variables))

      ;;The pattern is the :NOT special keyword.
      ((_ (:not ?not-pattern) ?kont ?bound-pattern-variables ?extracted-variables)
       #'(extract-new-pattern-variables ?not-pattern ?kont
					?bound-pattern-variables ?extracted-variables))

      ;;The pattern is a quoted S-expression.
      ((_ (quote x) (?kont ...) ?bound-pattern-variables ?extracted-variables)
       #'(?kont ... ?extracted-variables))

      ;;The pattern is a list whose second element is the ellipsis.
      ((_ (?first-pattern ?second-pattern . ?rest-patterns)
	  ?kont ?bound-pattern-variables ?extracted-variables)
       (and (identifier? #'?second-pattern) (free-identifier=? #'(... ...) #'?second-pattern))
       #'(extract-new-pattern-variables (?first-pattern . ?rest-patterns) ?kont
					?bound-pattern-variables ?extracted-variables))

      ;;The pattern is a list whose second element is NOT the ellipsis.
      ((_ (?first-pattern ?second-pattern . ?rest-pattern)
	  ?kont ?bound-pattern-variables ?extracted-variables)
       #'(extract-new-pattern-variables ?first-pattern
					(extract-new-cdr-pattern-variables
					 (?second-pattern . ?rest-pattern)
					 ?kont ?bound-pattern-variables ?extracted-variables)
					?bound-pattern-variables ()))

      ;;The pattern is a pair.
      ((_ (?first-pattern . ?second-pattern)
	  ?kont ?bound-pattern-variables ?extracted-variables)
       #'(extract-new-pattern-variables ?first-pattern
					(extract-new-cdr-pattern-variables
					 ?second-pattern ?kont
					 ?bound-pattern-variables ?extracted-variables)
					?bound-pattern-variables ()))

      ;;The pattern is a vector.
      ((_ #(?pattern ...) ?kont ?bound-pattern-variables ?extracted-variables)
       #'(extract-new-pattern-variables (?pattern ...)
					?kont ?bound-pattern-variables ?extracted-variables))

      ;;The pattern is the underscore identifier, the wildcard.
      ((_ ?pattern (?kont ...) ?bound-pattern-variables ?extracted-variables)
       (and (identifier? #'?pattern) (free-identifier=? #'_ #'?pattern))
       #'(?kont ... ?extracted-variables))

      ;;The pattern is a non-identifier literal.
      ((_ ?pattern (?kont ...) ?bound-pattern-variables ?extracted-variables)
       (not (identifier? #'?pattern))
       #'(?kont ... ?extracted-variables))

      ;;The pattern is an  identifier already bound as pattern variable:
      ;;we ignore it.   This is the only clause in which  we make use of
      ;;?BOUND-PATTERN-VARIABLE.
      ((_ ?pattern (?kont ...) (?bound-pattern-variable ...) ?extracted-variables)
       (and (identifier? #'?pattern)
	    (memq (syntax->datum #'?pattern) (syntax->datum #'(?bound-pattern-variable ...))))
       #'(?kont ... ?extracted-variables))

      ;;The pattern is  an identifier not bound as  pattern variable: we
      ;;register it in the list of extracted variables.
      ((_ ?pattern (?kont ...) ?bound-pattern-variables ?extracted-variables)
       (identifier? #'?pattern)
       #'(?kont ... (?pattern . ?extracted-variables)))

      ;;We should never reach this clause.
      ((_ ?pattern ?kont ?bound-pattern-variables ?extracted-variables)
       #'(syntax-violation 'match "unrecognised pattern element" ?pattern))

      )))

(define-syntax extract-new-cdr-pattern-variables
  ;;When EXTRACT-NEW-PATTERN-VARIABLES has  to parse a composite pattern
  ;;like:
  ;;
  ;;    (?car-pattern . ?cdr-pattern)
  ;;    ()
  ;;
  ;;it has  to process recursively the component  patterns; however when
  ;;processing ?CDR-PATTERN the variables  found in ?CAR-PATTERN must be
  ;;present as "already bound", so simply expanding:
  ;;
  ;;    (extract-new-pattern-variables
  ;;		(?car-pattern . ?cdr-pattern) ?kont
  ;;		?bound-pattern-variables ?extracted-variables)
  ;;
  ;;to:
  ;;
  ;;    (extract-new-pattern-variables
  ;;		?car-pattern
  ;;		(extract-new-pattern-variables
  ;;		    ?cdr-pattern ?kont
  ;;		    ?bound-pattern-variables ?extracted-variables)
  ;;		?bound-pattern-variables ?extracted-variables)
  ;;
  ;;is incorrect.  By using this macro to wrap the continuation:
  ;;
  ;;    (extract-new-pattern-variables
  ;;		?car-pattern
  ;;		(extract-new-cdr-pattern-variables
  ;;		    ?cdr-pattern ?kont
  ;;		    ?bound-pattern-variables ?extracted-variables)
  ;;		?bound-pattern-variables ?extracted-variables)
  ;;
  ;;we append the variables extracted  from the ?CAR-PATTERN to the list
  ;;of already bound variables when parsing the ?CDR-pattern.
  ;;
  (syntax-rules ()
    ((_ ?pattern ?kont ?bound-pattern-variables ?extracted-variables
	(?extracted-variable-from-car ...))
     (extract-new-pattern-variables ?pattern ?kont
				    (?extracted-variable-from-car ... . ?bound-pattern-variables)
				    (?extracted-variable-from-car ... . ?extracted-variables)))))


;;;; gimme some sugar baby

(define-syntax match-define
  (syntax-rules ()
    ((_ ?name ?clause ...)
     (define ?name (match-lambda ?clause ...)))))

(define-syntax match-define*
  (syntax-rules ()
    ((_ ?name ?clause ...)
     (define ?name (match-lambda* ?clause ...)))))

(define-syntax match-lambda
  (syntax-rules ()
    ((_ ?clause ...) (lambda (expr) (match expr ?clause ...)))))

(define-syntax match-lambda*
  (syntax-rules ()
    ((_ ?clause ...) (lambda expr (match expr ?clause ...)))))

(define-syntax match-let
  (syntax-rules ()
    ((_ (vars ...) . body)
     (%match-let/helper let () () (vars ...) . body))
    ((_ loop . rest)
     (match-named-let loop () . rest))))

(define-syntax match-let*
  (syntax-rules ()
    ((_ () . ?body)
     (begin . ?body))
    ((_ ((?pattern ?expr) . ?rest) . ?body)
     (match ?expr
       (?pattern (match-let* ?rest . ?body))))))

(define-syntax match-named-let
  (syntax-rules ()
    ((_ loop ((pat expr var) ...) () . body)
     (let loop ((var expr) ...)
       (match-let ((pat var) ...)
		  . body)))
    ((_ loop (v ...) ((pat expr) . rest) . body)
     (match-named-let loop (v ... (pat expr tmp)) rest . body))))

(define-syntax match-letrec
  (syntax-rules ()
    ((_ vars . body)
     (%match-let/helper letrec () () vars . body))))

(define-syntax %match-let/helper
  ;;To be called with the following arguments:
  ;;
  ;;LET		- The identifier of the selected LET form, LET
  ;;		  or LETREC.
  ;;
  ;;BINDINGS	- The list of bindings for the outer LET form
  ;;		  accumulated so far.
  ;;
  ;;CLAUSES	- The list of clauses accumulated so far.
  ;;
  ;;BODY	- The body to be evaluated if all the patterns
  ;;		  match all the expressions.
  ;;
  (syntax-rules ()

    ;;Possible initial and  final form, no more clauses  nor patterns to
    ;;process.  This rule matches only if the original syntax defined no
    ;;clauses, only simple bindings.
    ((_ ?let ((?var ?expr) ...) () () . ?body)
     (?let ((?var ?expr) ...) . ?body))

    ;;Possible  final  form,  no   more  clauses  but  patterns.   Start
    ;;MATCH-LET* recursion to nest pattern matchers.
    ((_ ?let ((?var ?expr) ...) ((?pattern ?tmp-var) ...) () . ?body)
     (?let ((?var ?expr) ...)
	   (match-let* ((?pattern ?tmp-var) ...) . ?body)))

    ;;Possible initial form: the pattern  in the first clause is a pair.
    ;;Generate a temporary  variable TMP for the expression  and add the
    ;;pair  pattern to  the list  of  patterns.  Notice  that this  also
    ;;matches  the  special  patterns  :predicate,  :accessor,  :getter,
    ;;:setter.
    ((_ ?let (?binding ...) (?pattern ...) (((?a . ?b) ?expr) . ?rest) . ?body)
     (%match-let/helper ?let (?binding ... (tmp ?expr))
			(?pattern ... ((?a . ?b) tmp))
			?rest . ?body))

    ;;Possible  initial form:  the  pattern  in the  first  clause is  a
    ;;vector.  Generate a temporary  variable TMP for the expression and
    ;;add the vector pattern to the list of patterns.
    ((_ ?let (?binding ...) (?pattern ...) ((#(?a ...) ?expr) . ?rest) . ?body)
     (%match-let/helper ?let (?binding ... (tmp ?expr))
			(?pattern ... (#(?a ...) tmp))
			?rest . ?body))

    ;;Possible initial form: the first clause is a list of two elements;
    ;;add the first clause to the list of bindings.
    ((_ ?let (?binding ...) (?pattern ...) ((?a ?expr) . ?rest) . ?body)
     (%match-let/helper ?let (?binding ... (?a ?expr))
			(?pattern ...)
			?rest . ?body))))


;;;; done

)

;;; end of file

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
;;;Copyright (c) 2006, 2007 Alex Shinn
;;;Nausicaa integration by Marco Maggi <marco.maggi-ipsu@poste.it>
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


;;;; helpers

(define-syntax if-ellipsis
  ;;If ?THING  is the ellipsis  identifier "...", expand to  the success
  ;;continuation; else expand to the failure continuation.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?thing ?success-kont ?failure-kont)
       (if (and (identifier? (syntax ?thing))
		(free-identifier=? (syntax ?thing)
				   (syntax (... ...))))
	   (syntax ?success-kont)
	 (syntax ?failure-kont))))))

(define-syntax verify-no-ellipsis
  ;;Raise a syntax error if the first argument is a list and any element
  ;;in it is an ellipsis identifier.
  ;;
  (syntax-rules ()
    ((_ (x . y) sk)
     (if-ellipsis x (syntax-violation 'match
			       "multiple ellipsis patterns not allowed at same level"
			       '(x . y))
			   (verify-no-ellipsis y sk)))
    ((_ x sk)
     sk)))

(define-syntax match-drop-ids
  ;;A continuation-passing-style utility that  takes two values and just
  ;;expands into the first.
  ;;
  (syntax-rules ()
    ((_ expr ids ...) expr)))


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
  (syntax-rules (=>)
    ((_)
     (syntax-violation 'match "missing match expression" '(match)))

    ((_ ?expr)
     (syntax-violation 'match "missing match clause" '(match ?expr)))

    ((_ ?expr (?pattern))
     (syntax-violation 'match "no body in match clause" '(?pattern)))

    ((_ ?expr (?pattern (=> ?failure)))
     (syntax-violation 'match "no body in match clause" '(?pattern (=> ?failure))))

    ((_ (?item ...) ?clause ...)
     (let ((expr (?item ...)))
       (next-clause expr expr (set! expr) ?clause ...)))

    ((_ #(?item ...) ?clause ...)
     (let ((expr (quote #(?item ...))))
       (next-clause expr expr (set! expr) ?clause ...)))

    ((_ ?atom ?clause ...)
     (next-clause ?atom ?atom (set! ?atom) ?clause ...))))


(define-syntax next-clause
  ;;Match an expression against the  full pattern from the first clause;
  ;;if matching fails, invoke a  thunk that attempts to match the second
  ;;clause or  raise an error  if no other  clauses are present.   To be
  ;;called with the following arguments:
  ;;
  ;;EXPR	- the expression to match
  ;;GETTER	- the getter form
  ;;SETTER	- the setter form
  ;;CLAUSE ...	- one or more match clauses
  ;;
  ;;The EXPR value must be the name of a temporary variable to which the
  ;;expression to match  is bound, or the expression itself  if it is an
  ;;atom.  EXPR is meant to be evaluated multiple times in the expansion
  ;;of the macros.
  ;;
  ;;The GETTER  must be a form  which, when evaluated,  returns the full
  ;;expression;  the getter  can be  EXPR itself.   It is  used  only by
  ;;DISPATCH-PATTERN when  the pattern  has the form  "(get! <getter>)",
  ;;where <getter> is a symbol; the usage looks like this:
  ;;
  ;;	(let ((<getter> (lambda () GETTER)))
  ;;	  BODY)
  ;;
  ;;and the  thunk bound to  <getter> is supposed  to be invoked  in the
  ;;BODY.
  ;;
  ;;The  SETTER  must  be an  "incomplete"  form;  it  is used  only  by
  ;;DISPATCH-PATTERN when  the pattern  has the form  "(set! <setter>)",
  ;;where <setter> is a symbol; the usage looks like this:
  ;;
  ;;	(let ((<setter> (lambda (x) (?setter ... x)))) BODY)
  ;;
  ;;and  the the  accessor function  bound to  <setter> is  meant  to be
  ;;invoked by  the BODY.  The SETTER  form with X appended  is meant to
  ;;result to  a form  which, when  evaluated, sets the  value X  in the
  ;;expression.
  ;;
  (syntax-rules (=>)

    ;;no more clauses
    ((_ ?expr ?getter ?setter)
     (match-mismatch-error 'match ?expr))

    ((_ ?expr ?getter ?setter (?pattern (=> ?failure) . ?body) . ?other-clauses)
     (let ((?failure (lambda ()
		       (next-clause ?expr ?getter ?setter . ?other-clauses))))
       (next-pattern ?expr ?pattern ?getter ?setter
		     (match-drop-ids (begin . ?body)) ;success continuation
		     (?failure) ;failure continuation
		     ())))	;matched identifiers

    ;;anonymous failure continuation, give it a dummy name
    ((_ ?expr ?getter ?setter (?pattern . ?body) . ?other-clauses)
     (next-clause ?expr ?getter ?setter (?pattern (=> failure) . ?body) . ?other-clauses))))


(define-syntax next-pattern
  ;;Match   the  expression  against   a  pattern   element;  it   is  a
  ;;normalisation step  before DISPATCH-PATTERN.  To be  called with the
  ;;following arguments:
  ;;
  ;;EXPR		- the expression to match
  ;;PATTERN		- is the current pattern to match against EXPR
  ;;GETTER		- the getter
  ;;SETTER		- the setter
  ;;SUCCESS-KONT	- the success continuation
  ;;FAILURE-KONT	- the failure continuation
  ;;IDENTIFIERS		- the list of identifiers bound in the pattern
  ;;			  so far
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
  (syntax-rules ()

    ((_ ?expr (?pattern ?second-pattern . ?pattern-rest)
	?getter ?setter ?success-kont ?failure-kont ?identifiers)
     (if-ellipsis ?second-pattern
		  (extract-vars ?pattern
				(ellipsis-pattern ?expr ?pattern ?pattern-rest
						  ?getter ?setter
						  ?success-kont ?failure-kont ?identifiers)
				?identifiers ())
		  (dispatch-pattern ?expr (?pattern ?second-pattern . ?pattern-rest)
				    ?getter ?setter
				    ?success-kont ?failure-kont ?identifiers)))

    ((_ . ?x)
     (dispatch-pattern . ?x))))


(define-syntax dispatch-pattern
  ;;Match a  pattern element against all the  supported pattern matching
  ;;models; invoke  the appropriate macro.   To be called with  the same
  ;;arguments of NEXT-PATTERN.
  ;;
  (lambda (stx)
    (syntax-case stx (:predicate :accessor :and :or :not quote quasiquote set! get!)

      ;;the pattern is null
      ((_ v () g s (sk ...) fk i)
       #'(if (null? v) (sk ... i) fk))

      ;;the pattern is a quoted identifier
      ((_ ?expr (quote ?pattern) g s (?success-kont ...) ?failure-kont ?identifiers)
       (identifier? #'?pattern)
       #'(if (eq? ?expr (quote ?pattern))
	     (?success-kont ... ?identifiers)
	   ?failure-kont))

      ;;the pattern is a quoted S-expression
      ((_ ?expr (quote ?pattern) g s (?success-kont ...) ?failure-kont ?identifiers)
       #'(if (equal? ?expr (quote ?pattern))
	     (?success-kont ... ?identifiers)
	   ?failure-kont))

      ;; ((_ ?expr (quote ?pattern) g s (?success-kont ...) ?failure-kont ?identifiers)
      ;;  #'(if-identifier ?pattern
      ;; 			(if (eq? ?expr (quote ?pattern))
      ;; 			    (?success-kont ... ?identifiers)
      ;; 			  ?failure-kont)
      ;; 			(if (equal? ?expr (quote ?pattern))
      ;; 			    (?success-kont ... ?identifiers)
      ;; 			  ?failure-kont)))

      ;;the pattern is a quasiquoted sexp
      ((_ ?expr (quasiquote ?pattern) g s (sk ...) fk (id ...))
       #'(let ((pattern (quasiquote ?pattern)))
	   (if (equal? ?expr pattern)
	       (sk ... (id ...))
	     fk)))

      ;;the pattern is the empty :AND
      ((_ v (:and) g s (sk ...) fk i)
       #'(sk ... i))

      ;;the pattern is a single-clause AND
      ((_ v (:and p) g s sk fk i)
       #'(next-pattern v p g s sk fk i))

      ;;the pattern is a non-empty AND
      ((_ ?expr (:and p q ...) g s sk fk i)
       #'(next-pattern ?expr p g s (next-pattern ?expr (:and q ...) g s sk fk) fk i))

      ;;the pattern is an empty OR
      ((_ v (:or) g s sk ?failure-kont i)
       #'?failure-kont)

      ;;the pattern is a single-clause OR
      ((_ v (:or p) g s sk fk i)
       #'(next-pattern v p g s sk fk i))

      ;;the pattern is a multiple-clause OR
      ((_ v (:or p ...) g s sk fk i)
       #'(extract-vars (:or p ...)
		       (generate-or v (p ...) g s sk fk i)
		       i
		       ()))

      ;;the pattern is a :NOT form
      ((_ v (:not p) g s (sk ...) fk i)
       #'(next-pattern v p g s (match-drop-ids fk) (sk ... i) i))

      ;;the pattern is an empty :NOT form
      ((_ v (:not) g s (sk ...) fk i)
       #'(syntax-violation 'match "empty :NOT form in pattern" '(:not)))

      ;;the pattern is a getter
      ((_ v (get! getter) ?getter s (?success-kont ...) fk ?identifiers)
       #'(let ((getter (lambda () ?getter)))
	   (?success-kont ... ?identifiers)))

      ;;the pattern is a setter
      ((_ v (set! setter) g (?setter ...) (?success-kont ...) fk ?identifiers)
       #'(let ((setter (lambda (x) (?setter ... x))))
	   (?success-kont ... ?identifiers)))

      ;;the pattern is a quasiquoted predicate
      ((_ ?expr (:predicate (quasiquote ?predicate) ?pattern ...) g s sk ?failure-kont i)
       #'(let ((predicate (quasiquote ?predicate)))
	   (if (predicate ?expr)
	       (next-pattern ?expr (:and ?pattern ...) g s sk ?failure-kont i)
	     ?failure-kont)))

      ;;the pattern is a predicate
      ((_ ?expr (:predicate ?predicate ?pattern ...) g s sk ?failure-kont i)
       #'(if (?predicate ?expr)
	     (next-pattern ?expr (:and ?pattern ...) g s sk ?failure-kont i)
	   ?failure-kont))

      ;;the pattern is an accessor matcher with quasiquoted procedure
      ((_ ?expr (:accessor (quasiquote ?proc) ?pattern) g s sk fk i)
       #'(let ((proc (quasiquote ?proc)))
	   (let ((expr1 (proc ?expr)))
	     (next-pattern expr1 ?pattern g s sk fk i))))

      ;;the pattern is an accessor matcher
      ((_ ?expr (:accessor ?proc ?pattern) g s sk fk i)
       #'(let ((expr1 (?proc ?expr)))
	   (next-pattern expr1 ?pattern g s sk fk i)))

      ;;the pattern is a one-element list
      ((_ ?expr (p) g s sk fk i)
       #'(if (and (pair? ?expr)
		  (null? (cdr ?expr)))
	     (let-syntax ((sub-expr (identifier-syntax (car ?expr))))
	       (next-pattern sub-expr p sub-expr (set-car! ?expr) sk fk i))
	     ;; (let ((sub-expr (car ?expr)))
	     ;;   (next-pattern sub-expr p sub-expr (set-car! ?expr) sk fk i))
	   fk))

      ;;the pattern is a pair
      ((_ ?expr (?pattern . ?pattern-rest) g s ?success-kont ?failure-kont ?identifiers)
       #'(if (pair? ?expr)
	     (let-syntax ((expr-a (identifier-syntax (car ?expr)))
			  (expr-d (identifier-syntax (cdr ?expr))))
	     ;; (let ((expr-a (car ?expr))
	     ;; 	   (expr-d (cdr ?expr)))
	       (next-pattern expr-a ?pattern (car ?expr) (set-car! ?expr)
			     (next-pattern expr-d ?pattern-rest ;success continuation
					   (cdr ?expr) (set-cdr! ?expr)
					   ?success-kont ?failure-kont)
			     ?failure-kont
			     ?identifiers))
	   ?failure-kont))

      ;;the pattern is a vector
      ((_ ?expr #(?pattern ...) g s sk fk i)
       #'(match-vector ?expr
		       0 ;index of the first element
		       () ;list of index patterns
		       (?pattern ...) sk fk i))

      ;;the pattern is the wildcard
      ((_ v underscore g s (sk ...) fk i)
       (and (identifier? #'underscore)
	    (eq? '_ (syntax->datum #'underscore)))
       #'(sk ... i))

      ;;Not a pair or vector or special literal, test to see if it's a new
      ;;symbol, in which case we just bind it, or if it's an already bound
      ;;symbol or  some other  literal, in which  case we compare  it with
      ;;EQUAL?.
      ((_ ?expr ?pattern g s (sk ...) fk (id ...))
       #'(let-syntax ((new-sym? (syntax-rules (id ...)
		;If ?PATTERN is among the ID symbols listed as literals,
		;the first rule matches; else the second rule matches.
				  ((_ ?pattern sk2 fk2) sk2)
				  ((_ y        sk2 fk2) fk2))))
	   (new-sym? random-sym-to-match
		     (let ((?pattern ?expr))
		       (sk ... (id ... ?pattern)))
		     (if (equal? ?expr ?pattern)
			 (sk ... (id ...))
		       fk))))
      )))


(define-syntax generate-or
  ;;Generating OR clauses just involves binding the success continuation
  ;;into a  thunk which takes the  identifiers from each  OR clause, and
  ;;trying each clause, calling the thunk as soon as we succeed.
  ;;
  (syntax-rules ()
    ((_ v p g s (sk ...) fk (i ...) ((id id-ls) ...))
     (let ((sk2 (lambda (id ...) (sk ... (i ... id ...)))))
       (generate-or-step v p g s
			  (match-drop-ids (sk2 id ...))
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
  ;;Match a pattern with an ellipsis.
  ;;
  (lambda (stx)
    (syntax-case stx ()

      ;;Match  the  pattern  "(?pattern   ...)"   when  ?PATTERN  is  an
      ;;identifier: bind the expression to it and call the continuation.
      ((_ ?expr ?pattern () ;pattern rest
	  ?getter ?setter
	  (?success-kont ...) ?failure-kont
	  ?identifiers ((id id-ls) ...))
       (identifier? #'?pattern)
       #'(let ((?pattern ?expr))
	   (?success-kont ... ?identifiers)))

      ;;Match  the pattern  "(?pattern  ...)"  when  ?PATTERN  is not  a
      ;;identifier:  match  the   nested  patterns  against  the  nested
      ;;expressions.
      ((_ ?expr ?pattern () ;pattern rest
	  ?getter ?setter
	  (?success-kont ...) ?failure-kont
	  ?identifiers ((id id-ls) ...))
       #'(let loop ((ls ?expr)
		    (id-ls '())
		    ...)
	   (cond ((null? ls)
		  (let ((id (reverse id-ls)) ...)
		    (?success-kont ... ?identifiers)))
		 ((pair? ls)
		  (let ((sub-expr (car ls)))
		    (next-pattern sub-expr ?pattern sub-expr (set-car! ls)
				  (match-drop-ids (loop (cdr ls) (cons id id-ls) ...))
				  ?failure-kont ?identifiers)))
		 (else
		  ?failure-kont))))

      ;;Match  the  pattern  "(?pattern  ...  .   ?pattern-rest)"  where
      ;;?PATTERN-REST are trailing patterns.
      ((_ ?expr ?pattern (?pattern-rest ...)
	  g s
	  (?success-kont ...) ?failure-kont
	  ?identifiers ((id id-ls) ...))
       #'(verify-no-ellipsis (?pattern-rest ...)
			     (let* ((tail-len (length '(?pattern-rest ...)))
				    (ls ?expr)
				    (len (length ls)))
			       (if (< len tail-len)
				   ?failure-kont
				 (let loop ((ls ls)
					    (n len)
					    (id-ls '())
					    ...)
				   (cond
				    ((= n tail-len)
				     (let ((id (reverse id-ls)) ...)
				       (next-pattern ls (?pattern-rest ...) #f #f
						     (?success-kont ... ?identifiers) ?failure-kont
						     ?identifiers)))
				    ((pair? ls)
				     (let ((w (car ls)))
				       (next-pattern w ?pattern (car ls) (set-car! ls)
						     (match-drop-ids
						      (loop (cdr ls) (- n 1) (cons id id-ls) ...))
						     ?failure-kont
						     ?identifiers)))
				    (else
				     ?failure-kont))))))))))


;;;; vector matching

(define-syntax match-vector
  ;;This syntax is  the entry point for vector matching,  but it is also
  ;;called recursively.  Synopsis:
  ;;
  ;;	(match-vector ?expression-to-be-matches
  ;;		      0		;index of the first element
  ;;                  ()	;list of index patterns
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
  ;;indexes in the vector (these are called "index patterns") and "3" is
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
      ((_ v ?next-index ?index-patterns (penultimate-pattern last-pattern) sk fk i)
       (and (identifier? #'last-pattern)
	    (free-identifier=? #'last-pattern #'(... ...)))
       #'(match-vector-ellipsis v ?next-index ?index-patterns penultimate-pattern sk fk i))

      ;;All the  patterns have been converted to  index patterns.  Check
      ;;the exact vector length, then match each element in turn.
      ((_ ?expr ?vector-len ((?pattern ?index) ...) () sk fk i)
       #'(if (vector? ?expr)
	     (let-syntax ((len (identifier-syntax (vector-length ?expr))))
	       (if (= len ?vector-len)
		   (match-vector-index-pattern ?expr ((?pattern ?index) ...) sk fk i)
		 fk))
	   fk))

      ;;Convert the next pattern into an index pattern.
      ((_ ?expr ?next-index (?index-pattern ...) (?pattern . ?pattern-rest) sk fk i)
       #'(match-vector ?expr (+ ?next-index 1)
		       (?index-pattern ... (?pattern ?next-index))
		       ?pattern-rest sk fk i))
      )))

(define-syntax match-vector-index-pattern
  ;;Expand to the  code needed to match the  next index pattern.  Expand
  ;;recursively until all the index patterns have been processed.
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
  (syntax-rules ()
    ((_ ?expr ?expr-vector-len ?ellipsis-pattern ?number-of-index-patterns sk fk i)
     (extract-vars ?ellipsis-pattern
		   (match-vector-tail #t
				      ?expr ?expr-vector-len
				      ?ellipsis-pattern ?number-of-index-patterns
				      sk fk i)
		   i ()))

    ((_ #t v ?expr-vector-len ?ellipsis-pattern ?number-of-index-patterns (sk ...) fk i ((id id-ls) ...))
     (let loop ((j     ?number-of-index-patterns) ;loop over the tail vector indices
		(id-ls '())
		...)
       (if (>= j ?expr-vector-len)
	   (let ((id (reverse id-ls))
		 ...)
	     (sk ... i))
         (let-syntax ((item (identifier-syntax (vector-ref v j))))
           (next-pattern item ?ellipsis-pattern
			 (vector-ref v j) (vetor-set! v j)
			 (match-drop-ids (loop (+ j 1) (cons id id-ls) ...)) ;success continuation
			 fk i)))))))


(define-syntax extract-vars
  ;;Extract  all  identifiers  in   a  pattern.   All  the  branches  in
  ;;DISPATCH-PATTERN must be duplicated here.
  ;;
  ;;A little more complicated than  just looking for symbols, we need to
  ;;ignore special keywords and not pattern forms (such as the predicate
  ;;expression in ?  patterns).
  ;;
  ;; (extract-vars pattern continuation (ids ...) (new-vars ...))
  ;;
  (lambda (stx)
    (syntax-case stx (:predicate :accessor :and :or :not quote quasiquote get! set!)
      ((_ (:predicate pred . p) k i v)
       #'(extract-vars p k i v))
      ((_ (:accessor accessor p) k i v)
       #'(extract-vars p k i v))
      ((_ (quote x) (k ...) i v)
       #'(k ... v))
      ((_ (:and . p) k i v)
       #'(extract-vars p k i v))
      ((_ (:or . p) k i v)
       #'(extract-vars p k i v))
      ((_ (:not . p) k i v)
       #'(extract-vars p k i v))
      ((_ (p q . r) k i v)
		;A non-keyword pair, expand  the CAR with a continuation
		;to expand the CDR.
       #'(if-ellipsis q
		      (extract-vars (p . r) k i v)
		      (extract-vars p (extract-vars-step (q . r) k i v) i ())))
      ((_ (p . q) k i v)
       #'(extract-vars p (extract-vars-step q k i v) i ()))
      ((_ #(p ...) k i v)
       #'(extract-vars (p ...) k i v))
      ((_ underscore (k ...) i v)
       (and (identifier? #'underscore)
	    (eq? '_ (syntax->datum #'underscore)))
       #'(k ... v))
      ((_ p (k ...) (i ...) v)
		;This is  the main part,  the only place where  we might
		;add a new var if it's an unbound symbol.
       #'(let-syntax ((new-sym? (syntax-rules (i ...)
				  ((_ p sk fk) sk)
				  ((_ x sk fk) fk))))
	   (new-sym? random-sym-to-match
		     (k ... ((p p-ls) . v))
		     (k ... v)))))))

(define-syntax extract-vars-step
  ;;Stepper  used  in  the above  so  it  can  expand  the CAR  and  CDR
  ;;separately.
  ;;
  (syntax-rules ()
    ((_ p k i v ((v2 v2-ls) ...))
     (extract-vars p k (v2 ... . i) ((v2 v2-ls) ... . v)))))


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
    ;;matches the special patterns :predicate, :accessor, get!, set!.
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

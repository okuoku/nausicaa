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
;;;Nausicaa integration by Marco Maggi <marcomaggi@gna.org>
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
    match match-lambda match-lambda*
    match-let match-letrec match-named-let match-let*)
  (import (rnrs)
    (rnrs mutable-pairs))


;;;; helpers

(define-syntax if-identifier
  ;;If "?thing"  is an identifier,  expand to the  success continuation;
  ;;else expand to the failure continuation.
  ;;
  (lambda (stx)
    (syntax-case stx ()
      ((_ ?thing ?success-kont ?failure-kont)
       (if (identifier? (syntax ?thing))
	   (syntax ?success-kont)
	 (syntax ?failure-kont))))))

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
     (let ((expr #(?item ...)))
       (next-clause expr expr (set! expr) ?clause ...)))

    ((_ ?atom ?clause ...)
     (next-clause ?atom ?atom (set! ?atom) ?clause ...))))


(define-syntax next-clause
  ;;Match  a full  pattern from  the  first clause;  if matching  fails,
  ;;invoke a thunk that attempts to  match the second clause or raise an
  ;;error  if no  other  clauses are  present.   To be  called with  the
  ;;following arguments:
  ;;
  ;;EXPR	- the expression to match
  ;;GETTER	- the getter
  ;;SETTER	- the setter
  ;;CLAUSE ...	- the match clauses
  ;;
  (syntax-rules (=>)

    ;;no more clauses
    ((_ ?expr ?getter ?setter)
     (syntax-violation 'match "no matching pattern" ?expr))

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

    ((_ ?expr (?p ?q . ?rest) ?getter ?setter ?success-kont ?failure-kont ?identifiers)
     (if-ellipsis ?q
		  (extract-vars ?p
				(ellipsis-pattern ?expr ?p ?rest ?getter ?setter
						  ?success-kont ?failure-kont
						  ?identifiers)
				?identifiers ())
		  (dispatch-pattern ?expr (?p ?q . ?rest) ?getter ?setter
				    ?success-kont ?failure-kont ?identifiers)))

    ((_ . ?x)
     (dispatch-pattern . ?x))))


(define-syntax dispatch-pattern
  ;;Match a  pattern element against all the  supported pattern matching
  ;;models; invoke  the appropriate macro.   To be called with  the same
  ;;arguments of NEXT-PATTERN.
  ;;
  (syntax-rules (quote quasiquote * ? $ and or not set! get!)

    ;;the pattern is null
    ((_ v () g s (sk ...) fk i)
     (if (null? v) (sk ... i) fk))

    ;;the pattern is a quoted sexp
    ((_ v (quote p) g s (sk ...) fk i)
     (if (equal? v 'p) (sk ... i) fk))

    ;;the pattern is a quasiquoted sexp
    ((_ v (quasiquote p) g s sk fk i)
     (match-quasiquote v p g s sk fk i))

    ;;the pattern is the empty AND
    ((_ v (and) g s (sk ...) fk i)
     (sk ... i))

    ;;the pattern is a single-clause AND
    ((_ v (and p) g s sk fk i)
     (next-pattern v p g s sk fk i))

    ;;the pattern is a non-empty AND
    ((_ v (and p q ...) g s sk fk i)
     (next-pattern v p g s (next-pattern v (and q ...) g s sk fk) fk i))

    ;;the pattern is an empty OR
    ((_ v (or) g s sk fk i)
     fk)

    ;;the pattern is a single-clause OR
    ((_ v (or p) g s sk fk i)
     (next-pattern v p g s sk fk i))

    ;;the pattern is a multiple-clause OR
    ((_ v (or p ...) g s sk fk i)
     (extract-vars (or p ...)
		   (generate-or v (p ...) g s sk fk i)
		   i
		   ()))

    ;;the pattern is a NOT form
    ((_ v (not p) g s (sk ...) fk i)
     (next-pattern v p g s (match-drop-ids fk) (sk ... i) i))

    ;;the pattern is an empty NOT form
    ((_ v (not) g s (sk ...) fk i)
     (syntax-violation 'match "empty NOT form in pattern" '(not)))

    ;;the pattern is a getter
    ((_ v (get! getter) g s (sk ...) fk i)
     (let ((getter (lambda () g)))
       (sk ... i)))

    ;;the pattern is a setter
    ((_ v (set! setter) g (s ...) (sk ...) fk i)
     (let ((setter (lambda (x) (s ... x))))
       (sk ... i)))

    ;;the pattern is a predicate
    ((_ v (? pred p ...) g s sk fk i)
     (if (pred v)
	 (next-pattern v (and p ...) g s sk fk i)
       fk))

    ;;the pattern is a transformed expression matcher
    ((_ ?expr ($ ?proc ?pattern) g s sk fk i)
     (let ((expr1 (?proc ?expr)))
       (next-pattern expr1 ?pattern g s sk fk i)))

    ;;the pattern is a one-element list
    ((_ ?expr (p) g s sk fk i)
     (if (and (pair? ?expr)
	      (null? (cdr ?expr)))
	 (let ((sub-expr (car ?expr)))
	   (next-pattern sub-expr p sub-expr (set-car! ?expr) sk fk i))
       fk))

    ;;the pattern is a pair
    ((_ ?expr (?pattern . ?pattern-rest) g s ?success-kont ?failure-kont ?identifiers)
     (if (pair? ?expr)
	 (let ((expr-a (car ?expr))
	       (expr-d (cdr ?expr)))
	   (next-pattern expr-a ?pattern (car ?expr) (set-car! ?expr)
			 (next-pattern expr-d ?pattern-rest ;success continuation
				       (cdr ?expr) (set-cdr! ?expr)
				       ?success-kont ?failure-kont)
			 ?failure-kont
			 ?identifiers))
       ?failure-kont))

    ;;the pattern is a vector
    ((_ v #(p ...) g s sk fk i)
     (if (vector? v)
	 (match-vector v 0 () (p ...) sk fk i)
       fk))

    ;;the pattern is the wildcard
    ((_ v * g s (sk ...) fk i)
     (sk ... i))

    ;;Not a pair or vector or special literal, test to see if it's a new
    ;;symbol, in which case we just bind it, or if it's an already bound
    ;;symbol or  some other  literal, in which  case we compare  it with
    ;;EQUAL?.
    ((_ ?expr ?pattern g s (sk ...) fk (id ...))
     (let-syntax ((new-sym? (syntax-rules (id ...)
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
    ))


(define-syntax generate-or
  ;;Generating OR clauses just involves binding the success continuation
  ;;into a thunk  which takes the identifiers common  to each OR clause,
  ;;and trying each clause, calling the thunk as soon as we succeed.
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
  (syntax-rules ()

    ;;Match the pattern "(?pattern ...)".  If ?PATTERN is an identifier,
    ;;bind the expression  to it and call the  continuation; if ?PATTERN
    ;;is  a   form  match  the   nested  patterns  against   the  nested
    ;;expressions.
    ((_ ?expr ?pattern () ?getter ?setter
	(?success-kont ...) ?failure-kont
	?identifiers ((id id-ls) ...))
     (if-identifier ?pattern
		    (let ((?pattern ?expr))
		      (?success-kont ... ?identifiers))
		    (let loop ((ls ?expr)
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
			     ?failure-kont)))))

    ;;Match  the  pattern "(?pattern  ...   .  ?rest)"  where ?REST  are
    ;;trailing patterns.
    ((_ ?expr ?pattern (?pattern-rest ...)
	g s (?success-kont ...) ?failure-kont
	?identifiers ((id id-ls) ...))
     (verify-no-ellipsis (?pattern-rest ...)
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
				 ?failure-kont)))))))))


(define-syntax match-quasiquote
  (syntax-rules (unquote unquote-splicing quasiquote)
    ((_ v (unquote p) g s sk fk i)
     (next-pattern v p g s sk fk i))
    ((_ v ((unquote-splicing p) . rest) g s sk fk i)
     (if (pair? v)
	 (next-pattern v
		       (p . tmp)
		       (match-quasiquote tmp rest g s sk fk)
		       fk
		       i)
       fk))
    ((_ v (quasiquote p) g s sk fk i . depth)
     (match-quasiquote v p g s sk fk i #f . depth))
    ((_ v (unquote p) g s sk fk i x . depth)
     (match-quasiquote v p g s sk fk i . depth))
    ((_ v (unquote-splicing p) g s sk fk i x . depth)
     (match-quasiquote v p g s sk fk i . depth))
    ((_ v (p . q) g s sk fk i . depth)
     (if (pair? v)
	 (let ((w (car v)) (x (cdr v)))
	   (match-quasiquote
	    w p g s
	    (match-quasiquote-step x q g s sk fk depth)
	    fk i . depth))
       fk))
    ((_ v #(elt ...) g s sk fk i . depth)
     (if (vector? v)
	 (let ((ls (vector->list v)))
	   (match-quasiquote ls (elt ...) g s sk fk i . depth))
       fk))
    ((_ v x g s sk fk i . depth)
     (next-pattern v 'x g s sk fk i))))

(define-syntax match-quasiquote-step
  (syntax-rules ()
    ((_ x q g s sk fk depth i)
     (match-quasiquote x q g s sk fk i . depth))))


(define-syntax match-vector
  ;;Vector patterns are just more of the same, with the slight exception
  ;;that we pass around the current vector index being matched.
  ;;
  (syntax-rules (___)
    ((_ v n pats (p q) sk fk i)
     (if-ellipsis q
		  (match-vector-ellipsis v n pats p sk fk i)
		  (match-vector-two v n pats (p q) sk fk i)))
    ((_ v n pats (p ___) sk fk i)
     (match-vector-ellipsis v n pats p sk fk i))
    ((_ . x)
     (match-vector-two . x))))

(define-syntax match-vector-two
  ;;Check the exact vector length, then check each element in turn.
  ;;
  (syntax-rules ()
    ((_ v n ((pat index) ...) () sk fk i)
     (if (vector? v)
	 (let ((len (vector-length v)))
	   (if (= len n)
	       (match-vector-step v ((pat index) ...) sk fk i)
	     fk))
       fk))
    ((_ v n (pats ...) (p . q) sk fk i)
     (match-vector v (+ n 1) (pats ... (p n)) q sk fk i))))

(define-syntax match-vector-step
  (syntax-rules ()
    ((_ v () (sk ...) fk i) (sk ... i))
    ((_ v ((pat index) . rest) sk fk i)
     (let ((w (vector-ref v index)))
       (next-pattern w pat (vector-ref v index) (vector-set! v index)
		     (match-vector-step v rest sk fk)
		     fk i)))))

(define-syntax match-vector-ellipsis
  ;;With a  vector ellipsis pattern we  first check to see  if the vector
  ;;length is at least the required length.
  ;;
  (syntax-rules ()
    ((_ v n ((pat index) ...) p sk fk i)
     (if (vector? v)
	 (let ((len (vector-length v)))
	   (if (>= len n)
	       (match-vector-step v ((pat index) ...)
				  (match-vector-tail v p n len sk fk)
				  fk i)
	     fk))
       fk))))

(define-syntax match-vector-tail
  (syntax-rules ()
    ((_ v p n len sk fk i)
     (extract-vars p (match-vector-tail-two v p n len sk fk i) i ()))))

(define-syntax match-vector-tail-two
  (syntax-rules ()
    ((_ v p n len (sk ...) fk i ((id id-ls) ...))
     (let loop ((j n) (id-ls '()) ...)
       (if (>= j len)
	   (let ((id (reverse id-ls)) ...) (sk ... i))
         (let ((w (vector-ref v j)))
           (next-pattern w p (vector-ref v j) (vetor-set! v j)
			 (match-drop-ids (loop (+ j 1) (cons id id-ls) ...))
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
  (syntax-rules (* ___ ? $ quote quasiquote and or not get! set!)
    ((_ (? pred . p) k i v)
     (extract-vars p k i v))
    ((_ ($ rec . p) k i v)
     (extract-vars p k i v))
    ((_ (quote x) (k ...) i v)
     (k ... v))
    ((_ (quasiquote x) k i v)
     (match-extract-quasiquote-vars x k i v (#t)))
    ((_ (and . p) k i v)
     (extract-vars p k i v))
    ((_ (or . p) k i v)
     (extract-vars p k i v))
    ((_ (not . p) k i v)
     (extract-vars p k i v))
    ((_ (p q . r) k i v)
		;A non-keyword pair, expand  the CAR with a continuation
		;to expand the CDR.
     (if-ellipsis q
		  (extract-vars (p . r) k i v)
		  (extract-vars p (extract-vars-step (q . r) k i v) i ())))
    ((_ (p . q) k i v)
     (extract-vars p (extract-vars-step q k i v) i ()))
    ((_ #(p ...) k i v)
     (extract-vars (p ...) k i v))
    ((_ * (k ...) i v)
     (k ... v))
    ((_ p (k ...) (i ...) v)
		;This is  the main part,  the only place where  we might
		;add a new var if it's an unbound symbol.
     (let-syntax ((new-sym? (syntax-rules (i ...)
			      ((_ p sk fk) sk)
			      ((_ x sk fk) fk))))
       (new-sym? random-sym-to-match
                 (k ... ((p p-ls) . v))
                 (k ... v))))))

(define-syntax extract-vars-step
  ;;Stepper  used  in  the above  so  it  can  expand  the CAR  and  CDR
  ;;separately.
  ;;
  (syntax-rules ()
    ((_ p k i v ((v2 v2-ls) ...))
     (extract-vars p k (v2 ... . i) ((v2 v2-ls) ... . v)))))

(define-syntax match-extract-quasiquote-vars
  (syntax-rules (quasiquote unquote unquote-splicing)
    ((_ (quasiquote x) k i v d)
     (match-extract-quasiquote-vars x k i v (#t . d)))
    ((_ (unquote-splicing x) k i v d)
     (match-extract-quasiquote-vars (unquote x) k i v d))
    ((_ (unquote x) k i v (#t))
     (extract-vars x k i v))
    ((_ (unquote x) k i v (#t . d))
     (match-extract-quasiquote-vars x k i v d))
    ((_ (x . y) k i v (#t . d))
     (match-extract-quasiquote-vars
      x
      (match-extract-quasiquote-vars-step y k i v d) i ()))
    ((_ #(x ...) k i v (#t . d))
     (match-extract-quasiquote-vars (x ...) k i v d))
    ((_ x (k ...) i v (#t . d))
     (k ... v))))

(define-syntax match-extract-quasiquote-vars-step
  (syntax-rules ()
    ((_ x k i v d ((v2 v2-ls) ...))
     (match-extract-quasiquote-vars x k (v2 ... . i)
				    ((v2 v2-ls) ... . v) d))))


;;;; gimme some sugar baby

(define-syntax match-lambda
  (syntax-rules ()
    ((_ clause ...) (lambda (expr) (match expr clause ...)))))

(define-syntax match-lambda*
  (syntax-rules ()
    ((_ clause ...) (lambda expr (match expr clause ...)))))

(define-syntax match-let
  (syntax-rules ()
    ((_ (vars ...) . body)
     (match-let/helper let () () (vars ...) . body))
    ((_ loop . rest)
     (match-named-let loop () . rest))))

(define-syntax match-letrec
  (syntax-rules ()
    ((_ vars . body) (match-let/helper letrec () () vars . body))))

(define-syntax match-let/helper
  (syntax-rules ()
    ((_ let ((var expr) ...) () () . body)
     (let ((var expr) ...) . body))
    ((_ let ((var expr) ...) ((pat tmp) ...) () . body)
     (let ((var expr) ...)
       (match-let* ((pat tmp) ...)
		   . body)))
    ((_ let (v ...) (p ...) (((a . b) expr) . rest) . body)
     (match-let/helper
      let (v ... (tmp expr)) (p ... ((a . b) tmp)) rest . body))
    ((_ let (v ...) (p ...) ((#(a ...) expr) . rest) . body)
     (match-let/helper
      let (v ... (tmp expr)) (p ... (#(a ...) tmp)) rest . body))
    ((_ let (v ...) (p ...) ((a expr) . rest) . body)
     (match-let/helper let (v ... (a expr)) (p ...) rest . body))))

(define-syntax match-named-let
  (syntax-rules ()
    ((_ loop ((pat expr var) ...) () . body)
     (let loop ((var expr) ...)
       (match-let ((pat var) ...)
		  . body)))
    ((_ loop (v ...) ((pat expr) . rest) . body)
     (match-named-let loop (v ... (pat expr tmp)) rest . body))))

(define-syntax match-let*
  (syntax-rules ()
    ((_ () . body)
     (begin . body))
    ((_ ((pat expr) . rest) . body)
     (match expr (pat (match-let* rest . body))))))


;;;; done

)

;;; end of file

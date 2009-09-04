;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (matches)
;;;Date: Sat Aug 29, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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


(import (nausicaa)
  (checks)
  (matches)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing matches\n")

(define-syntax catch-error
  (syntax-rules ()
    ((_ ?body)
     (guard (E ((irritants-condition? E)
		`((message   . ,(condition-message E))
		  (irritants . ,(condition-irritants E))))
	       (else
		#t))
       ?body))))

(define-syntax catch-syntax-error
  (syntax-rules ()
    ((_ ?body)
     (guard (E (else `((message   . ,(condition-message E))
		       (form      . ,(syntax-violation-form E)))))
       ?body))))

(define-syntax catch-mismatch-error
  (syntax-rules ()
    ((_ ?body)
     (guard (E (else `((message   . ,(condition-message E))
		       (expr      . ,(match-mismatch-expression E)))))
       ?body))))


(parametrise ((check-test-name 'errors))

  (check
      (catch-syntax-error (match))
    => '((message . "missing match expression")
	 (form    . (match))))

  (check
      (catch-syntax-error (match 123))
    => '((message . "missing match clause")
	 (form    . (match 123))))

  (check
      (catch-mismatch-error (match 28 (29 'ok)))
    => '((message . "no matching pattern")
	 (expr    . 28)))

  (check
      (catch-syntax-error (match 28 (28 (=> fail))))
    => '((message . "no body in match clause")
	 (form    . (28 (=> fail)))))

  (check
      (catch-syntax-error (match 28
			    ((a ... b ... c)
			     'fail)))
    => '((message . "multiple ellipsis patterns not allowed at same level")
	 (form    . (... c))))

  (check
      (catch-syntax-error (match 28
			    ((a ... b ...)
			     'fail)))
    => '((message . "multiple ellipsis patterns not allowed at same level")
	 (form    . (...))))

  (check
      (catch-syntax-error (match 28 (28)))
    => '((message . "no body in match clause")
	 (form    . (28))))

  #t)


(parameterise ((check-test-name 'wildcard))

  (check
      (match 'any (* 'ok))
    => 'ok)

  (check
      (match '(1 2)
	((* *)
	 'ok))
    => 'ok)

  #t)


(parameterise ((check-test-name 'variables))

  (check
      (match 'ok (x x))
    => 'ok)

  (check
      (match '_ (x x))
    => '_)

  (check
      (match '___ (x x))
    => '___)

  (check
      (match '... (x x))
    => '...)

  (check
      (let ((f (lambda args
		 (match args (x x)))))
	(f 1))
    => '(1))

  (check
      (let ((f (lambda (a)
		 (match a (x x)))))
	(f 1))
    => 1)

  (check
      (match '(1 2 3)
	(alpha (list alpha alpha alpha)))
    => '((1 2 3) (1 2 3) (1 2 3)))

  (check	;duplicate symbols pass
      (match '(ok . ok)
	((x . x) x))
    => 'ok)

  (check	;duplicate symbols fail
      (match '(ok . bad)
	((x . x) 'bad)
	(else    'ok))
    => 'ok)

  #t)


(parameterise ((check-test-name 'literals))

  (check	;number
      (match 28 (28 'ok))
    => 'ok)

  (check	;string
      (match "good"
	("bad" 'fail)
	("good" 'ok))
    => 'ok)

  (check	;string
      (match "bad"
	("bad" 'bad)
	("good" 'ok))
    => 'bad)

  (check	;literal symbol
      (match 'good
	('bad 'fail)
	('good 'ok))
    => 'ok)

  #t)


(parameterise ((check-test-name 'lists))

  (check	;null
      (match '()
	(() 'ok))
    => 'ok)

  (check	;pair
      (match '(ok)
	((x) x))
    => 'ok)

  (check	;list
      (match '(alpha (beta (delta 123)))
	((alpha (beta (delta x))) x))
    => 123)

  #t)


(parameterise ((check-test-name 'quoted))

  (check	;quoted sexp
      (match '(alpha (beta (delta 123)))
	('(alpha (beta (delta 123))) 1))
    => 1)

  (check	;quoted symbols
      (match '(alpha (beta (delta 123)))
	(('alpha ('beta ('delta x))) x))
    => 123)

  (check	;sexp
      (match '(alpha (beta (delta 123)))
	(('alpha (or ('beta ('delta x))
		     ('gamma ('delta x)))) x))
    => 123)

  (check	;sexp
      (match '(alpha (gamma (delta 123)))
	(('alpha (or ('beta ('delta x))
		     ('gamma ('delta x)))) x))
    => 123)

  #t)


(parameterise ((check-test-name 'vectors))

  (check
      (match '#()
	(#() 'ok))
    => 'ok)

  (check
      (match '#(ok)
	(#(x) x))
    => 'ok)

  (check
      (match '#(1 2 3 4)
	(#(x ...) x))
    => '(1 2 3 4))

  (check
      (match '#(1 2 3 4)
	(#(1 2 3 4) 'ok))
    => 'ok)

  (check
      (match 1
	(#(1 2 3 4)	'ok)
	(*		'fail))
    => 'fail)

  (check
      (match '#(1 2)
	(#(1 2 3 4)	'ok)
	(*		'fail))
    => 'fail)

  #t)


(parameterise ((check-test-name 'records))

  (define-record-type color
    (fields (immutable red)
	    (immutable green)
	    (immutable blue)))

  (check
      (match (make-color 1 2 3)
	((? color?)
	 'ok))
    => 'ok)

  (check
      (match (make-color 1 2 3)
	((? color? ($ color-red x))
	 x))
    => 1)

  (check
      (match (make-color 1 2 3)
	((? color?
	    ($ color-red   x)
	    ($ color-green y)
	    ($ color-blue  z))
	 (list x y z)))
    => '(1 2 3))

  (check
      (match (make-color 1 2 3)
	((? color?
	    ($ color-red (? zero?))) 'ok)
	(* 'fail))
    => 'fail)

  #t)


(parameterise ((check-test-name 'logic))

  (check	;and empty
      (match '(o k)
	((and) 'ok))
    => 'ok)

  (check	;and single
      (match 'ok
	((and x) x))
    => 'ok)

  (check	;and double
      (match 'ok
	((and (? symbol?) y)
	 'ok))
    => 'ok)

;;; --------------------------------------------------------------------

  (check	;or empty
      (match '(o k)
	((or) 'fail)
	(else 'ok))
    => 'ok)

  (check	;or single
      (match 'ok
	((or x) 'ok))
    => 'ok)

  (check	;or double
      (match 'ok
	((or (? symbol? y)
	     y)
	 y))
    => 'ok)

  (check
      (match 'ok
	((or (? integer? x)
	     (? symbol?  x))
	 x))
    => 'ok)

  (check
      (guard (E (else #t))
	(eval '(match 123
		 ((or (? integer? x)
		      (? symbol?  y))
		  y))
	      (environment '(rnrs) '(matches))))
    => #t)

  (check
      (guard (E (else #t))
	(eval '(match 123
		 ((or (? integer? x)
		      (? symbol?  y))
		  x))
	      (environment '(rnrs) '(matches))))
    => #t)

;;; --------------------------------------------------------------------

  (check	;not
      (match 28
	((not (a . b)) 'ok))
    => 'ok)

  (check	;not
      (match 28
	((not 28) 'fail)
	(*        'ok))
    => 'ok)

  (check
      (match 123
	((not (? symbol?))
	 'ok))
    => 'ok)

  (check
      (guard (E (else #t))
	(eval '(match 123
		 ((not (? symbol? x))
		  x)) ; unbound identifier
	      (environment '(rnrs) '(matches))))
    => #t)

  (check
      (catch-syntax-error (match '()
			    ((not) 'fail)))
    => '((message . "empty NOT form in pattern")
	 (form    . (not))))

  #t)


(parameterise ((check-test-name 'predicates))

  (check	;pred
      (match 28
	((? number?) 'ok))
    => 'ok)

  (check
      (match 28
	((? number? x)
	 (+ 1 x)))
    => 29)

  (check
      (match 28
	((? number? (? integer? x))
	 (+ 1 x)))
    => 29)

  (check
      (match 28
	((? number? x y z)
	 (list x y z)))
    => '(28 28 28))

  #t)


(parameterise ((check-test-name 'proc))

  (check
      (let ((f (lambda (x) (+ 1 x))))
	(match 1
	  (($ f x) 'ok)))
    => 'ok)

  (check
      (match 1
	(($ (lambda (x) (+ 1 x)) x)
	 x)
	(y y))
    => 2)

  #t)


(parameterise ((check-test-name 'quasiquote))

  (check
      (let ((x 1))
	(match 1
	  (`,x 'ok)
	  (*   'fail)))
    => 'ok)

  (check
      (let ((x 2))
	(match '(1 2 3)
	  ((* `,x y) y)
	  (*         'fail)))
    => 3)

  (check
      (let ((x 10))
	(match '(1 2 3)
	  ((* `,(- x 8) y)
	   y)
	  (*
	   'fail)))
    => 3)

  (check
      (let ((x '(2 3)))
	(match '(1 2 3 4)
	  (`(1 ,@x 4)	'ok)
	  (*		'fail)))
    => 'ok)

  (check
      (let ((pred number?))
	(match 28
	  ((? `,pred) 'ok)))
    => 'ok)

  (check
      (let ((f (lambda (x) (+ 1 x))))
	(match 2
	  (($ `,f x) x)))
    => 3)

  #t)


(parameterise ((check-test-name 'expand))

  (define-syntax it
    (syntax-rules ()
      ((_ ?var)
       (* ?var *))))

  (check
      (match '(1 2 3)
	((it x) x)
	(*      'fail))
    => 'fail)

  (check
      (let-syntax ((it (syntax-rules ()
			 ((_ ?var)
			  (* ?var *)))))
	(match '(1 2 3)
	  ((it x) x)
	  (*      'fail)))
    => 'fail)

  (check
      (let-syntax ((one (syntax-rules ()
			  ((_)
			   1)))
		   (two (syntax-rules ()
			  ((_ ?v)
			   (quote ?v)))))
	(two (one)))
    => '(one))


  #t)


(parameterise ((check-test-name 'ellipses))

  (check
      (match '()
	((x ...)
	 x))
    => '())

  (check
      (match '(a b c d)
	((x ...)
	 x))
    => '(a b c d))

  (check
      (match '(a b c d)
	((x y ...)
	 (list x y)))
    => '(a (b c d)))

  (check
      (match '#(a b c d)
	(#(x ...)
	 x))
    => '(a b c d))

  (check
      (match '#(a b c d)
	(#(x y ...)
	 (list x y)))
    => '(a (b c d)))

  (check
      (match '(a b c d)
	((x ... y)
	 (list x y)))
    => '((a b c) d))

  (check
      (match '(a b c d)
	((x ... y z)
	 (list x y z)))
    => '((a b) c d))

  (check
      (match '(a b c d)
	((x ... y z w)
	 (list x y z w)))
    => '((a) b c d))

  (check
      (match '(a b c d)
	((x ... y z w v)
	 (list x y z w v)))
    => '(() a b c d))

  (check
      (match '(a b c d)
	((x ... (y ...))
	 (list x y)))
    => '((a b c) d))

  (check
      (match '(a b (c d))
	((x ... (y ...))
	 (list x y)))
    => '((a b) (c d)))

  (check
      (match '(a b #(c d))
	((x ... #(y ...))
	 (list x y)))
    => '((a b) (c d)))

  ;;FIXME  Currently ellipsis  are allowed  only  as last  element in  a
  ;;pattern vector.
  ;;
  ;;   (check
  ;;       (match '#(a b #(c d))
  ;; 	     (#(x ... #(y ...))
  ;; 	      (list x y)))
  ;;     => '((a b) (c d)))

  (check
      ;;The "x"  in the  body is  bound to the  *list* of  elements that
      ;;matched "x" in the pattern.
      (match '((a . 1)
  	       (b . 2)
  	       (c . 3))
	(((x . y) ...)
	 (list x y)))
    => '((a b c)
	 (1 2 3)))

  (check
      (match '#(1 2 3 (a . 1) (b . 2) (c . 3))
	(#(a b c (hd . tl) ...)
	 (list a b c hd tl)))
    => '(1 2 3 (a b c) (1 2 3)))

  (check
      (match '(1 2 3)
	(((? odd? n) ...) ;does not match, not all odd
	 n)
	(((? even? n) ...) ;does not match, not all even
	 n)
	(((? number? n) ...) ;does match, all numbers
	 n))
    => '(1 2 3))

  #t)


(parameterise ((check-test-name 'continuation))

  (check
      (match '(1 2)
	((a . b)
	 (=> next)
	 (if (even? a) 'fail (next)))
	((a . b)
	 'ok))
    => 'ok)

  (check
      (match 3
	((? positive? x)
	 (=> next)
	 (if (even? x)
	     x
	   (next)))
	(* 0))
    => 0)

  #t)


(parameterise ((check-test-name 'extensions))

  (check
      (match-let ()
	1)
    => 1)

  (check
      (match-let ((x 1))
	x)
    => 1)

  (check
      (match-let ((* 1))
	*)
    => 1)

  (check
      (match-let ((* 1))
	1)
    => 1)

  (check
      (match-let ((x 'ok)
		  (y '(o k)))
	y)
    => '(o k))

  (check
      (match-let (((x . y) '(1 . 2))
		  (z       3))
	(list x y z))
    => '(1 2 3))

  (check
      (match-let (((? number?  x) 1)
		  ((? integer? y) 2))
	x)
    => 1)

;;; --------------------------------------------------------------------

  (check
      (match-let* ()
	1)
    => 1)

  (check
      (match-let* ((x 1))
	x)
    => 1)

  (check
      (match-let* ((* 1))
	1)
    => 1)

  (check
      (match-let* ((x 'ok)
		   (y '(o k)))
	y)
    => '(o k))

  (check
      (match-let* (((x . y) '(1 . 2))
		   (z       3))
	(list x y z))
    => '(1 2 3))

  (check
      (match-let* (((? number?  x) 1)
		   ((? integer? y) 2))
	x)
    => 1)

  (check
      (match-let* ((x 'f)
		   (y 'o)
		   ((z w) (list y x)))
	(list x y z w))
    => '(f o o f))

;;; --------------------------------------------------------------------

  (check
      (match-letrec ()
		    1)
    => 1)

  (check
      (match-letrec ((x 1))
		    x)
    => 1)

  (check
      (match-letrec ((* 1))
		    1)
    => 1)

  (check
      (match-letrec ((x 'ok)
		     (y '(o k)))
		    y)
    => '(o k))

  (check
      (match-letrec (((x . y) '(1 . 2))
		     (z       3))
		    (list x y z))
    => '(1 2 3))

  (check
      (match-letrec (((? number?  x) 1)
		     ((? integer? y) 2))
		    x)
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let ((f (match-lambda ((x y) (+ x y)))))
	(f '(1 2)))
    => 3)

  (check
      (let ((f (match-lambda* ((x y) (+ x y)))))
	(f 1 2))
    => 3)

  (let ()
    (match-define one
      ((x y) (+ x y)))

    (match-define* two
      ((x y) (+ x y)))

    (check
	(one '(1 2))
      => 3)

    (check
	(two 1 2)
      => 3)

    )

  #t)


(parameterise ((check-test-name 'getter-setter))

  (check
      (match 2
	((get! two)
	 (two)))
    => 2)

  (check
      (match '(2)
	((get! two)
	 (two)))
    => '(2))

  (check
      (match '(2)
	(((get! two))
	 (two)))
    => 2)

  (check
      (match '(1 2 3)
	((* (get! two) *)
	 (two)))
    => 2)

  (check	;getter car
      (match '(1 . 2)
	(((get! a) . b)
	 (list (a) b)))
    => '(1 2))

  (check	;getter cdr
      (match '(1 . 2)
	((a . (get! b))
	 (list a (b))))
    => '(1 2))

  (check	;getter vector
      (match '#(1 2 3)
	(#((get! a) b c)
	 (list (a) b c)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (let ((x 1))
	(match x
	  ((set! doit)
	   (doit 3)))
	x)
    => 3)

  (check
      (catch-error (eval '(match 1
			    ((and (set! doit)
				  x)
			     (doit 3)
			     x))
			 (environment '(rnrs) '(matches))))
    => #t)

  (check	;setter car
      (let ((x '(1 . 2)))
	(match x (((set! a) . b) (a 3)))
	x)
    => '(3 . 2))

  (check	;setter car
      (let ((x '(1 . 2)))
	(match x (((set! a) . *) (a 3)))
	x)
    => '(3 . 2))

  (check	;setter cdr
      (let ((x '(1 . 2)))
	(match x ((a . (set! b)) (b 3)))
	x)
    => '(1 . 3))

  (check
      (let ((x '(1 2 3 4)))
	(match x ((a b (set! c) d)
		  (c 10)))
	x)
    => '(1 2 10 4))

  (check	;setter vector
      (let ((x '#(1 2 3)))
	(match x (#(a (set! b) c) (b 0)))
	x)
    => '#(1 0 3))

  #t)


;;;; done

(check-report)

;;; end of file

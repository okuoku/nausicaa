;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for sexp
;;;Date: Fri Aug 28, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (sentinel)
  (sexps)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing sexp\n")

(define-syntax catch-error
  (syntax-rules ()
    ((_ ?form ...)
     (guard (E ((sexp-mismatch-condition? E)
		`((message . ,(condition-message     E))
		  (pattern . ,(condition-sexp-mismatch/pattern E))
		  (form    . ,(condition-sexp-mismatch/form    E)))))
       ?form ...))))


(parameterise ((check-test-name 'plain))

  (check
      (sexp-match '(a b c)
		  '(a b c))
    => '())

  (check
      (sexp-match '(a 1 #t)
		  '(a 1 #t))
    => '())

;;; --------------------------------------------------------------------

  (check
      (catch-error (sexp-match '()
			       '(a b c)))
    => '((message . "S-expressions mismatch")
	 (pattern . ())
	 (form    . (a b c))))

  (check
      (catch-error (sexp-match '(a b c)
			       '()))
    => '((message . "S-expressions mismatch")
	 (pattern . a)
	 (form    . ())))

  (check
      (catch-error (sexp-match '(a b c)
			       '(a b d)))
    => '((message . "S-expressions mismatch")
	 (pattern . c)
	 (form    . d)))

  (check
      (catch-error (sexp-match '(a b c)
			       '(a d e)))
    => '((message . "S-expressions mismatch")
	 (pattern . b)
	 (form    . d)))

  #t)


(parameterise ((check-test-name 'var))

  (let-sexp-variables ((?v 1))

    (check
	(sexp-match `(,(sexp-var ?v))
		    '(a))
      => `((,?v . a)))

    (check
	(sexp-match `(a b ,(sexp-var ?v))
		    '(a b d))
      => `((,?v . d)))

    (check
	(sexp-match `(a ,(sexp-var ?v) d)
		    '(a b d))
      => `((,?v . b)))

    (check
	(sexp-match `(a ,(sexp-var ?v) d)
		    '(a (1 (2 (3 (4 5)))) d))
      => `((,?v . (1 (2 (3 (4 5)))))))

;;; --------------------------------------------------------------------

    (check
	(sexp-match `(,(sexp-var-rest ?v))
		    '(a b c d e f))
      => `((,?v . (a b c d e f))))

;;; --------------------------------------------------------------------

    (check
	(catch-error (sexp-match `(,(sexp-var ?v))
				 '()))
      => `((message . "S-expressions mismatch")
	   (pattern . (sexp-var ,?v))
	   (form    . ())))

    #t)

  #t)


(parameterise ((check-test-name 'pred))

  (check
      (sexp-match `(,(sexp-pred integer?)) '(123))
    => '())

  (check
      (sexp-match `(,(sexp-pred null?)) '())
    => '())

  (check
      (sexp-match `(((,(sexp-pred integer?)))) '(((123))))
    => '())

;;; --------------------------------------------------------------------

  (check
      (catch-error (sexp-match `(,(sexp-pred integer?)) '()))
    => `((message . "S-expressions mismatch")
	 (pattern . (sexp-pred ,integer?))
	 (form    . ())))

  (check
      (catch-error (sexp-match `(,(sexp-pred integer?)) '(1.2)))
    => `((message . "S-expressions mismatch")
	 (pattern . (sexp-pred ,integer?))
	 (form    . 1.2)))

  #t)


(parameterise ((check-test-name 'logic-or))

  (check (sexp-match `(,(sexp-or 'a 'b 'c)) '(a)) => '())
  (check (sexp-match `(,(sexp-or 'a 'b 'c)) '(b)) => '())
  (check (sexp-match `(,(sexp-or 'a 'b 'c)) '(c)) => '())

  (check (sexp-match `(,(sexp-or '(a) '(b) '(c))) '((a))) => '())
  (check (sexp-match `(,(sexp-or '(a) '(b) '(c))) '((b))) => '())
  (check (sexp-match `(,(sexp-or '(a) '(b) '(c))) '((c))) => '())

  (check (sexp-match `(,(sexp-or* a b c)) '(a)) => '())
  (check (sexp-match `(,(sexp-or* a b c)) '(b)) => '())
  (check (sexp-match `(,(sexp-or* a b c)) '(c)) => '())

  (check (sexp-match `(,(sexp-or* (a) (b) (c))) '((a))) => '())
  (check (sexp-match `(,(sexp-or* (a) (b) (c))) '((b))) => '())
  (check (sexp-match `(,(sexp-or* (a) (b) (c))) '((c))) => '())

  (check (sexp-match `(,(sexp-or* a b c) d) '(a d)) => '())
  (check (sexp-match `(,(sexp-or* a b c) d) '(b d)) => '())
  (check (sexp-match `(,(sexp-or* a b c) d) '(c d)) => '())

  (let-sexp-variables ((?d sentinel))
    (check (sexp-match `(,(sexp-or* a b c) ,(sexp-var ?d)) '(a d)) => `((,?d . d)))
    (check (sexp-match `(,(sexp-or* a b c) ,(sexp-var ?d)) '(b d)) => `((,?d . d)))
    (check (sexp-match `(,(sexp-or* a b c) ,(sexp-var ?d)) '(c d)) => `((,?d . d)))
    #t)

  (check (sexp-match `(a ,(sexp-or* b1 b2 b3) c) '(a b1 c)) => '())
  (check (sexp-match `(a ,(sexp-or* b1 b2 b3) c) '(a b2 c)) => '())
  (check (sexp-match `(a ,(sexp-or* b1 b2 b3) c) '(a b3 c)) => '())

  (let-sexp-variables ((?b2 sentinel))
    ;;Here  "b3" can never  be matched  literally because  "?b2" matches
    ;;everything.
    (check (sexp-match `(a ,(sexp-or 'b1 (sexp-var ?b2) 'b3) c) '(a b1 c)) => `())
    (check (sexp-match `(a ,(sexp-or 'b1 (sexp-var ?b2) 'b3) c) '(a 99 c)) => `((,?b2 . 99)))
    (check (sexp-match `(a ,(sexp-or 'b1 (sexp-var ?b2) 'b3) c) '(a b3 c)) => `((,?b2 . b3)))

    (check (sexp-match `(a ,(sexp-or 'b1 `(,(sexp-var ?b2)) 'b3) c) '(a b1   c)) => '())
    (check (sexp-match `(a ,(sexp-or 'b1 `(,(sexp-var ?b2)) 'b3) c) '(a (99) c)) => `((,?b2 . 99)))
    (check (sexp-match `(a ,(sexp-or 'b1 `(,(sexp-var ?b2)) 'b3) c) '(a b3   c)) => '())

    #f)

  (check (sexp-match `((,(sexp-or* a b c) d))   '((a d)))   => '())
  (check (sexp-match `(((,(sexp-or* a b c) d))) '(((a d)))) => '())

  (check (sexp-match `((1 (,(sexp-or* a b c) d))) '((1 (a d)))) => '())
  (check (sexp-match `(1 ((,(sexp-or* a b c) d))) '(1 ((a d)))) => '())

;;; --------------------------------------------------------------------

  (check
      (catch-error (sexp-match `(,(sexp-or* a b c)) '(d)))
    => '((message . "S-expressions mismatch")
	 (pattern . (sexp-or a b c))
	 (form    . d)))

  (check
      ;;The pattern  is the  closure returned by  SEXP-OR, so  we cannot
      ;;include it in the expected result.
      (guard (E ((sexp-mismatch-condition? E)
		 `((message . ,(condition-message  E))
		   (form    . ,(condition-sexp-mismatch/form E)))
		 ))
	(sexp-match `(,(sexp-or)) '()))
    => '((message . "S-expressions mismatch")
	 (form    . ())))

  (check
      ;;An OR with no  alternatives matches nothing.  This is consistent
      ;;with "(or)" returning #f in R6RS.
      (catch-error (sexp-match `(,(sexp-or)) '(a)))
    => '((message . "S-expressions mismatch")
	 (pattern . (sexp-or))
	 (form    . a)))

  (check
      (catch-error (sexp-match `(,(sexp-or* a)) '()))
    => '((message . "S-expressions mismatch")
	 (pattern . (sexp-or a))
	 (form    . ())))

  #f)


(parameterise ((check-test-name 'logic-and))

  ;;An AND with no  alternatives matches everything.  This is consistent
  ;;with "(and)" returning #t in R6RS.
  (check (sexp-match `(,(sexp-and)) '())  => '())
  (check (sexp-match `(,(sexp-and)) '(a)) => '())

  (check (sexp-match `(,(sexp-and) b)   '(a b))   => '())
  (check (sexp-match `(a ,(sexp-and) c) '(a b c)) => '())

  (check (sexp-match `(,(sexp-and 'a))             '(a))   => '())
  (check (sexp-match `(,(sexp-and 'a 'a 'a))       '(a))   => '())
  (check (sexp-match `(,(sexp-and '(a) '(a) '(a))) '((a))) => '())

  (check (sexp-match `(,(sexp-and* a))           '(a))   => '())
  (check (sexp-match `(,(sexp-and* a a a))       '(a))   => '())
  (check (sexp-match `(,(sexp-and* (a) (a) (a))) '((a))) => '())

  (let-sexp-variables ((?x sentinel)
		       (?y sentinel))
    (check
	(sexp-match `(,(sexp-and 'a (sexp-var ?y)))
		    '(a))
      => `((,?y . a)))

    (check
	(sexp-match `(,(sexp-and 'a
				 (sexp-var ?x)
				 (sexp-var ?y)))
		    '(a))
      => `((,?x . a)
	   (,?y . a)))

    (check
	(sexp-match `(,(sexp-and (sexp-pred integer?)
				 (sexp-var ?x)))
		    '(123))
      => `((,?x . 123)))

    (check
	(sexp-match `(,(sexp-and (sexp-pred integer?)
				 (sexp-pred positive?)
				 (sexp-var ?x)))
		    '(123))
      => `((,?x . 123)))

;;; --------------------------------------------------------------------

  (check
      (catch-error (sexp-match `(,(sexp-and* a)) '()))
    => '((message . "S-expressions mismatch")
	 (pattern . a)
	 (form    . ())))

    (check
	(catch-error (sexp-match `(,(sexp-and 'a (sexp-var ?y))) '(b)))
      => '((message . "S-expressions mismatch")
	   (pattern . a)
	   (form    . b)))

    (check
	(catch-error (sexp-match `(,(sexp-and (sexp-var ?y) 'a)) '(b)))
      => '((message . "S-expressions mismatch")
	   (pattern . a)
	   (form    . b)))

    #t)

  #t)


(parameterise ((check-test-name 'logic-any))

  (check (sexp-match `(,(sexp-any* a)) '())		=> '())
  (check (sexp-match `(,(sexp-any* a)) '(a))		=> '())
  (check (sexp-match `(,(sexp-any* a)) '(a a))		=> '())
  (check (sexp-match `(,(sexp-any* a)) '(a a a))	=> '())

  (check (sexp-match `(a ,(sexp-any* b)) '(a))		=> '())

  (check (sexp-match `(,(sexp-any* a) b) '(b))		=> '())
  (check (sexp-match `(,(sexp-any* a) b) '(a b))	=> '())
  (check (sexp-match `(,(sexp-any* a) b) '(a a b))	=> '())
  (check (sexp-match `(,(sexp-any* a) b) '(a a a b))	=> '())

  (check (sexp-match `(,(sexp-any* a) b)   '(a a a b))	 => '())
  (check (sexp-match `((,(sexp-any* a)) b) '((a a a) b)) => '())

  (check
      (sexp-match `(,(sexp-any (sexp-or* (a) (b) (c))) d)
  		  '((a) (b) (c) (a) (b) (c) d))
    => '())

  (let-sexp-variables ((?a sentinel) (?b sentinel)
		       (?c sentinel) (?d sentinel))
    (check
	(sexp-match `(doit ,(sexp-any (sexp-or `(alpha ,(sexp-var ?a))
					       `(beta ,(sexp-any (sexp-or `(delta ,(sexp-var ?b))
									  `(gamma ,(sexp-var ?c)))))
					       `(rho ,(sexp-var ?d)))))
		    '(doit (rho 4)
			   (alpha 1)
			   (beta (gamma 3)
				 (delta 2))))
      => `((,?d . 4)
	   (,?a . 1)
	   (,?c . 3)
	   (,?b . 2))))

;;; --------------------------------------------------------------------

  (check
      ;;Here SEXP-ANY matches  nothing in "(b)", so it  returns "(b)" it
      ;;self as  rest form.  After this,  the sexp is  finished; that is
      ;;"()" does not match "(b)", so an error is raised.
      (catch-error (sexp-match `(,(sexp-any* a)) '(b)))
    => '((message . "S-expressions mismatch")
	 (pattern . ())
	 (form    . (b))))

  #t)


(parameterise ((check-test-name 'logic-one))

  (check (sexp-match `(,(sexp-one* a)) '(a))		=> '())
  (check (sexp-match `(,(sexp-one* a)) '(a a))		=> '())
  (check (sexp-match `(,(sexp-one* a)) '(a a a))	=> '())

  (check (sexp-match `(a ,(sexp-one* b)) '(a b))	=> '())
  (check (sexp-match `(a ,(sexp-one* b)) '(a b b))	=> '())
  (check (sexp-match `(a ,(sexp-one* b)) '(a b b b))	=> '())

  (check (sexp-match `(,(sexp-one* a) b) '(a b))	=> '())
  (check (sexp-match `(,(sexp-one* a) b) '(a a b))	=> '())
  (check (sexp-match `(,(sexp-one* a) b) '(a a a b))	=> '())

  (check (sexp-match `(,(sexp-one* a) b)   '(a a a b))	 => '())
  (check (sexp-match `((,(sexp-one* a)) b) '((a a a) b)) => '())

  (check
      (sexp-match `(,(sexp-one (sexp-or* (a) (b) (c))) d)
  		  '((a) (b) (c) (a) (b) (c) d))
   => '())

  (let-sexp-variables ((?a sentinel) (?b sentinel)
		       (?c sentinel) (?d sentinel))
    (check
	(sexp-match `(doit ,(sexp-one (sexp-or `(alpha ,(sexp-var ?a))
					       `(beta ,(sexp-one (sexp-or `(delta ,(sexp-var ?b))
									  `(gamma ,(sexp-var ?c)))))
					       `(rho ,(sexp-var ?d)))))
		    '(doit (rho 4)
			   (alpha 1)
			   (beta (gamma 3)
				 (delta 2))))
      => `((,?d . 4)
	   (,?a . 1)
	   (,?c . 3)
	   (,?b . 2))))

;;; --------------------------------------------------------------------

  (check
      (catch-error (sexp-match `(,(sexp-one* a)) '(b)))
    => '((message . "S-expressions mismatch")
	 (pattern . a)
	 (form    . b)))

  (check
      (catch-error (sexp-match `(a ,(sexp-one* b)) '(a)))
    => '((message . "S-expressions mismatch")
	 (pattern . b)
	 (form    . ())))

  (check
      (catch-error (sexp-match `(,(sexp-one* a) b) '(b)))
    => '((message . "S-expressions mismatch")
	 (pattern . a)
	 (form    . b)))

  (check
      (catch-error (sexp-match `(,(sexp-one* a)) '()))
    => '((message . "S-expressions mismatch")
	 (pattern . a)
	 (form    . ())))

  #t)


(parameterise ((check-test-name 'ops-examples))

  (define (match-nothing form)
    (values '() form))

  (define (sexp-symbol symbol)
    (lambda (form)
      (cond ((null? form)
	     (sexp-mismatch-error #f symbol form))
	    ((eq? symbol (car form))
	     (values '() (cdr form)))
	    (else
	     (sexp-mismatch-error #f symbol form)))))

  (check (sexp-match `(a b ,match-nothing) '(a b)) => '())
  (check (sexp-match `(,match-nothing)     '())    => '())

  (check
      (sexp-match `(a b ,(sexp-symbol 'alpha))
		  '(a b alpha))
    => '())

  (check
      (sexp-match `(,(sexp-symbol 'alpha) ,(sexp-symbol 'beta))
		  '(alpha beta))
    => '())

  #t)


(parameterise ((check-test-name 'transform))

  (let ((f (make-sexp-transformer '(a b c)
				  `(1 2 3))))
    (check
	(f '(a b c))
      => '(1 2 3))

    #t)

  (let-sexp-variables ((?a sentinel) (?b sentinel)
		       (?c sentinel) (?d sentinel))
    (let ((f (make-sexp-transformer
	      `(doit ,(sexp-one (sexp-or `(alpha ,(sexp-var ?a))
					 `(beta ,(sexp-one (sexp-or `(delta ,(sexp-var ?b))
								    `(gamma ,(sexp-var ?c)))))
					 `(rho ,(sexp-var ?d)))))
	      `(doit ,?a ,?b ,?c ,?d))))
      (check
	  (f '(doit (rho 4)
		    (alpha 1)
		    (beta (gamma 3)
			  (delta 2))))
	=> '(doit 1 2 3 4))

      (check
	  ;;Here the ONE  closure matches the RHO element,  then it does
	  ;;not match ALPHAX; so it returns "((alphax ..." as rest form.
	  ;;Finally "()" does not match the rest form.
	  (catch-error (f '(doit (rho 4)
				 (alphax 1)
				 ;;    ^ !!!
				 (beta (gamma 3)
				       (delta 2)))))
	=> '((message . "S-expressions mismatch")
	     (pattern . ())
	     (form    . ((alphax 1)
			 (beta (gamma 3)
			       (delta 2))))))

      #f))

  (let-sexp-variables ((?c sentinel)
		       (?d sentinel)
		       (?e 123))
    (let ((f (make-sexp-transformer `(a (b ,(sexp-var ?c)))
				    `(out ,?d))))
      (check	;unbound, raise an error
	  (guard (E (else (condition-message E)))
	    (f '(a (b 1))))
	=> "unbound variable \"?d\" in output S-expression"))

    (let ((f (make-sexp-transformer `(a b)
				    `(out ,?e))))
      (check	;unbound, use the default
	  (f '(a b))
	=> `(out 123)))

    (let ((f (make-sexp-transformer `(a ,(sexp-or `(b ,(sexp-var ?c))
						  `(d ,(sexp-var ?d))))
				    `(out ,?d))))
      (check
	  (f '(a (d 1)))
	=> '(out 1))

      (check
	  ;;The transformer binds "1" to "?c", but the output form wants
	  ;;"?d".
	  (guard (E (else `((message   . ,(condition-message   E))
			    (irritants . ,(condition-irritants E)))))
	    (f '(a (b 1))))
	=> `((message   . "unbound variable \"?d\" in output S-expression")
	     ;;Remember that the irritants is a list.
	     (irritants . ((out ,?d)))))))

  (let ()

    (define trans
      (let-sexp-variables ((?a 10)
			   (?b 20))
	(make-sexp-transformer
	 `(doit ,(sexp-any (sexp-or `(alpha ,(sexp-var ?a))
				    `(beta  ,(sexp-var ?b)))))
	 `(doit ,?a ,?b))))

    (check
	(trans '(doit (alpha 1) (beta 2)))
      => '(doit 1 2))

    (check
	(trans '(doit (alpha 1)))
      => '(doit 1 20))

    (check
	(trans '(doit (beta 2)))
      => '(doit 10 2))

    (check
	(trans '(doit))
      => '(doit 10 20))

    #f)

  #t)


;;;; done

(check-report)

;;; end of file

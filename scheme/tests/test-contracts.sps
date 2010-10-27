;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for contracts
;;;Date: Mon Oct 25, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (nausicaa)
  (contracts)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing contracts\n")


(parametrise ((check-test-name	'variables))

  (let ()

    (define %alpha 123)

    (define-contract alpha %alpha integer?)

    (check alpha => 123)

    (check
	(begin
	  (set! alpha 456)
	  alpha)
      => 456)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (set! alpha #\a))
      => #t)

    #f)

  (let ()

    (define %beta "ciao")

    (define-contract beta %beta integer?)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  beta)
      => #t)

    #f)

  #t)


(parametrise ((check-test-name	'functions))

  (let ()	;no return value check
    (define (%doit a b c)
      (list a b c))

    (define-contract doit
	%doit
      (integer? string? symbol?))

    (check
	(doit 1 "two" 'three)
      => '(1 "two" three))

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit #\a "two" 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 2 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 "two" 3))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;return value check

    (define flag
      (make-parameter #t))

    (define (%doit a b c)
      (if (flag)
	  (list a b c)
	(vector a b c)))

    (define-contract doit
	%doit
      (integer? string? symbol? -> list?))

    (check
	(doit 1 "two" 'three)
      => '(1 "two" three))

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit #\a "two" 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 2 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 "two" 3))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (write E)(newline)
		   #t)
		  (else E))
	  (parametrise ((flag #f))
            (doit 1 "two" 'three)))
      => #t)

    ;; (parametrise ((flag #f))
    ;;   (doit 1 "two" 'three))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;multiple return value check

    (define flag
      (make-parameter #t))

    (define (%doit a b c)
      (if (flag)
	  (values a (list b c))
	(values a (vector b c))))

    (define-contract doit
	%doit
      (integer? string? symbol? -> integer? list?))

    (check
	(call-with-values
	    (lambda () (doit 1 "two" 'three))
	  list)
      => '(1 ("two" three)))

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (call-with-values
	      (lambda ()
		(doit #\a "two" 'three))
	    list))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (call-with-values
	      (lambda () (doit 1 2 'three))
	    list))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (call-with-values
	      (lambda () (doit 1 "two" 3))
	    list))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
;;;		   (write E)(newline)
		   #t)
		  (else E))
	  (parametrise ((flag #f))
	    (call-with-values
		(lambda () (doit 1 "two" 'three))
	      list)))
      => #t)

    ;; (parametrise ((flag #f))
    ;;   (doit 1 "two" 'three))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;no arguments check

    (define flag
      (make-parameter #t))

    (define (%doit)
      (if (flag)
	  '(1 "two" three)
	'#(1 "two" three)))

    (define-contract doit
	%doit
      (-> list?))

    (check
	(doit)
      => '(1 "two" three))

    (check
	(guard (E ((assertion-violation? E)
;;;		   (write E)(newline)
		   #t)
		  (else E))
	  (parametrise ((flag #f))
            (doit 1 "two" 'three)))
      => #t)

    ;; (parametrise ((flag #f))
    ;;   (doit 1 "two" 'three))

    #f)

;;; --------------------------------------------------------------------

  (let ()	;overcome the no-rest-argument limitation

    (define-contract doit
	%doit
      (integer? integers? -> vector?))

    (define (integers? ell)
      (for-all integer? ell))

    (define (%doit n ell)
      (vector n ell))

    (check
	(doit 1 (list 2 3 4))
      => '#(1 (2 3 4)))

    (check
    	(apply (lambda (n . args)
    		 (doit n args))
    	       '(1 2 3 4))
      => '#(1 (2 3 4)))

    #f)

  #t)


(parametrise ((check-test-name	'define/contract))

  (let ()	;function with no internal body substitutions
    (define/contract (doit a b c)
      (integer? string? symbol? -> list?)
      (list a b c))

    (check
	(doit 1 "two" 'three)
      => '(1 "two" three))

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit #\a "two" 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 2 'three))
      => #t)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 1 "two" 3))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;function with internal body substitutions

    (define (pred x)
      (or (not x) (integer? x)))

    (define/contract (doit n)
      (integer? -> pred)
      (if (zero? n)
	  #f
	(doit (- n 1))))

    (check
	(doit 10)
      => #f)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 'ciao))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;variable with expression

    (define/contract alpha integer? 123)

    (check alpha => 123)

    (check
	(begin
	  (set! alpha 456)
	  alpha)
      => 456)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (set! alpha #\a))
      => #t)

    #f)

  (let ()	;variable with function expression

    (define/contract alpha
      (integer? -> integer?)
      (lambda (n)
	(+ 1 n)))

    (check (alpha 123) => 124)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (alpha #\a))
      => #t)

    #f)

  (let ()	;variable without expression

    (define/contract alpha integer?)

    (check
	(begin
	  (set! alpha 456)
	  alpha)
      => 456)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (set! alpha #\a))
      => #t)

    #f)


  #t)


(parametrise ((check-test-name	'outer-contracts))

  (let ()

    (define (pred x)
      (or (not x) (integer? x)))

    (with-outer-contracts ((doit (integer? -> pred)))
      (define (doit n)
	(if (zero? n)
	    #f
	  (doit (- n 1)))))

    (check
	(doit 10)
      => #f)

    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (doit 'ciao))
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (with-outer-contracts ((a (integer? -> integer?))
			   (b (integer? -> integer?)))
      (define (a n)
	(if (zero? n)
	    n
	  (b (- n 1))))
      (define (b n)
	(cond ((zero? n)
	       n)
	      ((= 123 n)
	       #f)
	      (else
	       (a (- n 1)))))
      (check (b 123) => #f))

    (check (a 10) => 0)
    (check
	(guard (E ((assertion-violation? E)
		   #t)
		  (else E))
	  (b 123))
      => #t)

    #f)

  #t)


(parametrise ((check-test-name	'let-contract/variables))

  (let ((alpha 123)) ;single contract
    (let-contract ((alpha alpha integer?))
      (check alpha => 123)
      (check
	  (begin
	    (set! alpha 456)
	    alpha)
	=> 456)
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (set! alpha #\a))
	=> #t)
      #f)
    #f)

  (let ((%beta "ciao"))	;reference assertion violation on initial value
    (let-contract ((beta %beta integer?))
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    beta)
	=> #t))
    #f)

  (let ((alpha 123) ;multiple contracts
	(beta  "ciao"))
    (let-contract ((alpha alpha integer?)
		   (beta  beta  string?))
      (check alpha => 123)
      (check
	  (begin
	    (set! alpha 456)
	    alpha)
	=> 456)
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (set! alpha #\a))
	=> #t)
      (check beta => "ciao")
      (check
	  (begin
	    (set! beta "hello")
	    beta)
	=> "hello")
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (set! beta #\a))
	=> #t)
      #f)
    #f)

  #t)


(parametrise ((check-test-name	'let-contract/functions))

  (let ()	;no return value check
    (define (%doit a b c)
      (list a b c))

    (let-contract ((doit %doit (integer? string? symbol?)))

      (check
	  (doit 1 "two" 'three)
	=> '(1 "two" three))

      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (doit #\a "two" 'three))
	=> #t)

      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (doit 1 2 'three))
	=> #t)

      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (doit 1 "two" 3))
	=> #t)

      #f)
    #f)

;;; --------------------------------------------------------------------

  (let ()	;return value check
    (define flag
      (make-parameter #t))
    (define (%doit a b c)
      (if (flag)
	  (list a b c)
	(vector a b c)))
    (let-contract ((doit %doit (integer? string? symbol? -> list?)))
      (check
	  (doit 1 "two" 'three)
	=> '(1 "two" three))
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (doit #\a "two" 'three))
	=> #t)
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (doit 1 2 'three))
	=> #t)
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (doit 1 "two" 3))
	=> #t)
      (check
	  (guard (E ((assertion-violation? E)
;;;		   (write E)(newline)
		     #t)
		    (else E))
	    (parametrise ((flag #f))
	      (doit 1 "two" 'three)))
	=> #t)
      #f)
    #f)

;;; --------------------------------------------------------------------

  (let ()	;multiple return value check
    (define flag
      (make-parameter #t))
    (define (%doit a b c)
      (if (flag)
	  (values a (list b c))
	(values a (vector b c))))
    (let-contract ((doit %doit (integer? string? symbol? -> integer? list?)))
      (check
	  (call-with-values
	      (lambda () (doit 1 "two" 'three))
	    list)
	=> '(1 ("two" three)))
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (call-with-values
		(lambda ()
		  (doit #\a "two" 'three))
	      list))
	=> #t)
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (call-with-values
		(lambda () (doit 1 2 'three))
	      list))
	=> #t)
      (check
	  (guard (E ((assertion-violation? E)
		     #t)
		    (else E))
	    (call-with-values
		(lambda () (doit 1 "two" 3))
	      list))
	=> #t)
      (check
	  (guard (E ((assertion-violation? E)
;;;		   (write E)(newline)
		     #t)
		    (else E))
	    (parametrise ((flag #f))
	      (call-with-values
		  (lambda () (doit 1 "two" 'three))
		list)))
	=> #t)
      #f)
    #f)

;;; --------------------------------------------------------------------

  (let ()	;no arguments check
    (define flag
      (make-parameter #t))
    (define (%doit)
      (if (flag)
	  '(1 "two" three)
	'#(1 "two" three)))

    (let-contract ((doit %doit (-> list?)))
      (check
	  (doit)
	=> '(1 "two" three))
      (check
	  (guard (E ((assertion-violation? E)
;;;		   (write E)(newline)
		     #t)
		    (else E))
	    (parametrise ((flag #f))
	      (doit 1 "two" 'three)))
	=> #t)
      #f)
    #f)

  #t)


;;;; done

(check-report)

;;; end of file

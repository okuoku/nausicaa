;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (language-extensions)
;;;Date: Wed Nov 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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

;;;Copyright (c) 2008-2010 Derick Eddington
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
;;;Except  as  contained  in  this  notice, the  name(s)  of  the  above
;;;copyright holders  shall not be  used in advertising or  otherwise to
;;;promote  the sale,  use or  other dealings  in this  Software without
;;;prior written authorization.
;;;
;;;THE  SOFTWARE IS  PROVIDED "AS  IS",  WITHOUT WARRANTY  OF ANY  KIND,
;;;EXPRESS OR  IMPLIED, INCLUDING BUT  NOT LIMITED TO THE  WARRANTIES OF
;;;MERCHANTABILITY,    FITNESS   FOR    A    PARTICULAR   PURPOSE    AND
;;;NONINFRINGEMENT.  IN NO EVENT  SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;BE LIABLE  FOR ANY CLAIM, DAMAGES  OR OTHER LIABILITY,  WHETHER IN AN
;;;ACTION OF  CONTRACT, TORT  OR OTHERWISE, ARISING  FROM, OUT OF  OR IN
;;;CONNECTION  WITH THE SOFTWARE  OR THE  USE OR  OTHER DEALINGS  IN THE
;;;SOFTWARE.


#!r6rs
(import (for (nausicaa) run expand)
  (rnrs eval (6))
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing simple language extensions\n")

;; Here we use  EVAL because a syntax violation  error cannot be catched
;; by GUARD, and so it causes the program termination.
(define-syntax check-syntax-violation
  (syntax-rules ()
    ((_ ?form)
     (check
	 (guard (exc (else
;; 		      (write exc)(newline)
;; 		      (write (syntax-violation? exc))(newline)
		      (syntax-violation? exc)))
	   (eval '?form (environment '(nausicaa))))
       => #t))))


(parametrise ((check-test-name	'begin0))

  (check
      (begin0
	  (list 1 2)
	(list 3 4))
    => '(1 2))

  (check
      (call-with-values
	  (lambda ()
	    (begin0
		(values 1 2)
	      (values 3 4)))
	(lambda (a b)
	  (list a b)))
    => '(1 2))

;;; --------------------------------------------------------------------

  (check
      (begin0-let ((a 123))
	(list 1 2 3))
    => 123)

  (check
      (begin0-let ((a 123))
	(set! a 456))
    => 456)

  (check
      (let-values (((d e f) (begin0-let (((a b c) (values 1 2 3)))
			      (list 'a 'b 'c))))
	(list d e f))
    => '(1 2 3))

  #t)


(parametrise ((check-test-name	'loops))

  (check
      (with-result
       (dotimes (i 3)
	 1	; shooting the breeze
	 2	; shooting the breeze
	 (add-result i)))
    => '(#f (0 1 2)))

  (check
      (with-result
       (dotimes (i 3 (+ 2 4))
	 1	; shooting the breeze
	 2	; shooting the breeze
	 (add-result i)))
    => '(6 (0 1 2)))

;;; ------------------------------------------------------------

  (check
      (with-result
       (dolist (i '(1 2 3) (+ 2 4))
	 1	; shooting the breeze
	 2	; shooting the breeze
	 (add-result i)))
    => '(6 (1 2 3)))

  (check
      (with-result
       (dolist (i '(1 2 3))
	 1	; shooting the breeze
	 2	; shooting the breeze
	 (add-result i)))
    => '(#f (1 2 3)))

;;; ------------------------------------------------------------

  (check
      (with-result
       (loop-upon-list (item '(1 2 3 4))
	   (break-when #f)
	 (+ 1 2) ; shooting the breeze
	 (+ 3 4) ; shooting the breeze
	 (add-result item)))
    => '(#f (1 2 3 4)))

  (check
      (with-result
       (loop-upon-list (item '(1 2 3 4) (+ 2 4))
	   (break-when #f)
	 (+ 1 2) ; shooting the breeze
	 (+ 3 4) ; shooting the breeze
	 (add-result item)))
    => '(6 (1 2 3 4)))

  (check
      (with-result
       (loop-upon-list (item '(1 2 3 4))
	   (break-when (= item 3))
	 (+ 1 2) ; shooting the breeze
	 (+ 3 4) ; shooting the breeze
	 (add-result item)))
    => '(#f (1 2)))

;;; --------------------------------------------------------------------

  (check
      ((recursion (loop n)
	 (if (zero? n)
	     1
	   (* n (loop (- n 1)))))
       5)
    => 120)

;;; --------------------------------------------------------------------

  (check
      (let ((i 0))
	(while (< i 3)
	  (incr! i))
	i)
    => 3)

  (check
      (let ((i 0))
	(while (< 3 i)
	  (incr! i))
	i)
    => 0)

;;; --------------------------------------------------------------------

  (check
      (let ((i 0))
	(while* (< i 3)
	  (incr! i))
	i)
    => 3)

  (check
      (let ((i 0))
	(while* (< 3 i)
	  (incr! i))
	i)
    => 0)

  (check
      (let ((i 0))
	(while* (< i 3)
	  (break 4)))
    => 4)

  (check
      (let ((i 0))
	(receive (a b)
	    (while* (< i 3)
	      (break 4 5))
	  (list a b)))
    => '(4 5))

  (check
      (let ((i 0))
	(while* (< i 3)
	  (break))
	#f)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((i 0))
	(do-while (< i 3)
	  (incr! i))
	i)
    => 3)

  (check
      (let ((i 0))
	(do-while (< 3 i)
	  (incr! i))
	i)
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let ((i 0))
	(do-while* (< i 3)
	  (incr! i))
	i)
    => 3)

  (check
      (let ((i 0))
	(do-while* (< 3 i)
	  (incr! i))
	i)
    => 1)

  (check
      (let ((i 0))
	(do-while* (< i 3)
	  (break 4)))
    => 4)

  (check
      (let ((i 0))
	(receive (a b)
	    (do-while* (< i 3)
	      (break 4 5))
	  (list a b)))
    => '(4 5))

  (check
      (let ((i 0))
	(do-while* (< i 3)
	  (break))
	#f)
    => #f)

  #t)


(parametrise ((check-test-name	'ensure))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2))
	   (else
	    (add-result 3)
	    999))))
    => '(999 (1 2 3)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2))
	   (else
	    (add-result 3)
	    999))))
    => '(999 (1 2 3)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2)
	      (set! flag 1)
	      999)
	   (else
	    (add-result 3)
	    (add-result 4)))))
    => '(999 (1 2)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2)
	      (set! flag 1)
	      123)
	   (else
	    (add-result 3)
	    (add-result 4)))))
    => '(123 (1 2)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2))
	   (else-by
	    (add-result 3)
	    (add-result 4))
	   (else
	    (add-result 5)
	    (add-result 6)
	    (set! flag 1)
	    999))))
    => '(999 (1 2 3 4 5 6)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2))
	   (else-by
	    (add-result 3)
	    (add-result 4)
	    (set! flag 1)
	    999)
	   (else
	    (add-result 5)
	    (add-result 6)))))
    => '(999 (1 2 3 4)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2)
	      (set! flag 1)
	      999)
	   (else-by
	    (add-result 3)
	    (add-result 4))
	   (else
	    (add-result 5)
	    (add-result 6)))))
    => '(999 (1 2)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2))
	   (else-by
	    (add-result 3)
	    (add-result 4))
	   (else-by
	    (add-result 5)
	    (add-result 6))
	   (else
	    (add-result 7)
	    (add-result 8)
	    (set! flag 1)
	    999))))
    => '(999 (1 2 3 4 5 6 7 8)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2))
	   (else-by
	    (add-result 3)
	    (add-result 4))
	   (else-by
	    (add-result 5)
	    (add-result 6)
	    (set! flag 1)
	    999)
	   (else
	    (add-result 7)
	    (add-result 8)))))
    => '(999 (1 2 3 4 5 6)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2))
	   (else-by
	    (add-result 3)
	    (add-result 4)
	    (set! flag 1)
	    999)
	   (else-by
	    (add-result 5)
	    (add-result 6))
	   (else
	    (add-result 7)
	    (add-result 8)))))
    => '(999 (1 2 3 4)))

  (check
      (with-result
       (let ((flag 0))
	 (ensure (= flag 1)
	     (by
	      (add-result 1)
	      (add-result 2)
	      (set! flag 1)
	      999)
	   (else-by
	    (add-result 3)
	    (add-result 4))
	   (else-by
	    (add-result 5)
	    (add-result 6))
	   (else
	    (add-result 7)
	    (add-result 8)))))
    => '(999 (1 2)))

  #t)


(parametrise ((check-test-name	'unwind-protect))

  (check
      (unwind-protect
	  2
	(values))
    => 2)

  (check
      (let ((a 1))
	(unwind-protect
	    (set! a 2)
	  (set! a 3))
	a)
    => 3)

  (check
      (let ((a 1))
	(guard (E (else a))
	  (unwind-protect
	      (error #t "ciao")
	    (set! a 3))))
    => 3)

  #f)


(parametrise ((check-test-name	'bindings))

  (check
      (let ()
	(define-values (a b c)
	  #t
	  (values 1 2 3))
	(list a b c))
    => '(1 2 3))

  (check
      (let ()
	(define-values (a)
	  #t
	  (values 1))
	a)
    => 1)

  (check
      (let ()
	(define-values (a)
	  #t
	  1)
	a)
    => 1)

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define-constant a 123)
	a)
    => 123)

  (check
      (guard (E ((syntax-violation? E)
		 #t)
		(else #f))
	(eval '(letrec ()
		 (define-constant a 123)
		 (set! a 4)
		 a)
	      (environment '(nausicaa))))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ()
	(define-constant-values (a b c)
	  #t
	  (values 1 2 3))
	(list a b c))
    => '(1 2 3))

  (check
      (let ()
	(define-constant-values (a)
	  #t
	  (values 1))
	a)
    => 1)

  (check
      (let ()
	(define-constant-values (a)
	  #t
	  1)
	a)
    => 1)

  #t)


(parametrise ((check-test-name	'and-let))

  (check
      (and-let* ((a	#t)
		 (b	#t))
	#t)
    => #t)

  (check
      (and-let* ((a	#t)
		 (b	#f))
	#t)
    => #f)

  (check
      (and-let* ((a	#t)
		 (	#t))
	#t)
    => #t)

  (check
      (and-let* ((a	#f)
		 (	#t))
	#t)
    => #f)

;;; --------------------------------------------------------------------

  (check
      (and-let* () 1)
    => 1)

  (check
      (and-let* () 1 2)
    => 2)

  (check
      (and-let* () )
    => #t)

;;; --------------------------------------------------------------------

  (check
      (let ((x #f))
	(and-let* (x)))
    => #f)

  (check
      (let ((x 1))
	(and-let* (x)))
    => 1)

  (check
      (and-let* ((x #f)) )
    => #f)

  (check
      (and-let* ((x 1)) )
    => 1)

  (check-syntax-violation
   (and-let* (#f
	      (x 1))))

  (check
      (and-let* ( (#f) (x 1)) )
    => #f)

  (check-syntax-violation
   (and-let* (2 (x 1))))

  (check
      (and-let* ( (2) (x 1)) )
    => 1)

  (check
      (and-let* ( (x 1) (2)) )
    => 2)

  (check
      (let ((x #f))
	(and-let* (x) x))
    => #f)

  (check
      (let ((x ""))
	(and-let* (x) x))
    => "")

  (check
      (let ((x ""))
	(and-let* (x)))
    => "")

  (check
      (let ((x 1))
	(and-let* (x)
	  (+ x 1)))
    => 2)

  (check
      (let ((x #f))
	(and-let* (x)
	  (+ x 1)))
    => #f)

  (check
      (let ((x 1))
	(and-let* (((positive? x)))
	  (+ x 1)))
    => 2)

  (check
      (let ((x 1))
	(and-let* (((positive? x)))
	  ))
    => #t)

  (check
      (let ((x 0))
	(and-let* (((positive? x)))
	  (+ x 1)))
    => #f)

  (check
      (let ((x 1))
	(and-let* ((  (positive? x))
		   (x (+ x 1)))
	  (+ x 1)))
    => 3)

;;; This next one is from the reference implementation tests but I can't
;;; see how it "must be a syntax-error" (Derick Eddington).
  ;; (check-syntax-violation
  ;;  (let ((x 1))
  ;;    (and-let* ((  (positive? x))
  ;; 	      (x (+ x 1))
  ;; 	      (x (+ x 1)))
  ;;      (+ x 1))))

  (check
      (let ((x 1))
	(and-let* (x
		   ((positive? x)))
	  (+ x 1)))
    => 2)

  (check
      (let ((x 1))
	(and-let* (((begin x))
		   ((positive? x)))
	  (+ x 1)))
    => 2)

  (check
      (let ((x 0))
	(and-let* (x
		   ((positive? x)))
	  (+ x 1)))
    => #f)

  (check
      (let ((x #f))
	(and-let* (x
		   ((positive? x)))
	  (+ x 1)))
    => #f)

  (check
      (let ((x #f))
	(and-let* (((begin x))
		   ((positive? x)))
	  (+ x 1)))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (let ((x 1))
	(and-let* (x
		   (y (- x 1))
		   (  (positive? y)))
	  (/ x y)))
    => #f)

  (check
      (let ((x 0))
	(and-let* (x
		   (y (- x 1))
		   (  (positive? y)))
	  (/ x y)))
    => #f)

  (check
      (let ((x #f))
	(and-let* (x
		   (y (- x 1))
		   (  (positive? y)))
	  (/ x y)))
    => #f)

  (check
      (let ((x 3))
	(and-let* (x
		   (y (- x 1))
		   (  (positive? y)))
	  (/ x y)))
    => 3/2)

;;; --------------------------------------------------------------------

  (check-syntax-violation (and-let* (("oops" 1))))
  (check-syntax-violation (and-let* ((x 1 2))))
  (check-syntax-violation (and-let* ((x 1) . oops)))

  (check
      (let ((x 1))
	(and-let* ((x (+ x 1))
		   (x (+ x 1))
		   (x (+ x 1)))
	  (+ x 1)))
    => 5)

  (check
      (and-let* ()
	(define x 1)
	(- x))
    => -1)

  (check
      (and-let* ((x 2)
		 (y (+ 1 x)))
	(define z (* x y))
	(/ z))
    => 1/6)

  #t)


(parametrise ((check-test-name	'identifier-syntax))

  (check
      (let ((a 1))
  	(define-identifier-accessor-mutator alpha a
  	  (lambda (x) x) set!)
  	alpha)
    => 1)

  (check
      (let ((a 1))
  	(define-identifier-accessor-mutator alpha a
  	  (lambda (x) x))
  	alpha)
    => 1)

  (check
      (let ((a 1))
  	(define-identifier-accessor-mutator alpha a
  	  (lambda (x) x) set!)
  	(set! alpha 2)
  	alpha)
    => 2)

;;; --------------------------------------------------------------------

  (check
      (let ((a 1))
  	(with-accessor-and-mutator ((alpha a (lambda (x) x) set!))
  				   alpha))
    => 1)

  (check
      (let ((a 1))
  	(with-accessor-and-mutator ((alpha a (lambda (x) x) set!))
  				   (set! alpha 2)
  				   alpha))
    => 2)

  (check
      (let ((a 1))
  	(with-accessor-and-mutator ((alpha a (lambda (x) x)))
  				   alpha))
    => 1)

  (check
      (let ((a 1)
  	    (b 2))
  	(with-accessor-and-mutator ((alpha a (lambda (x) x) set!)
  				    (beta  b (lambda (x) x) set!))
  				   (list alpha beta)))
    => '(1 2))

  (check
      (let ((a 1)
  	    (b 2))
  	(with-accessor-and-mutator ((alpha a (lambda (x) x) set!)
  				    (beta  b (lambda (x) x) set!))
  				   (set! alpha 3)
  				   (set! beta  4)
  				   (list alpha beta)))
    => '(3 4))

  (check
      (let ((a 1)
  	    (b 2))
  	(with-accessor-and-mutator ((alpha a (lambda (x) x))
  				    (beta  b (lambda (x) x)))
  				   (list alpha beta)))
    => '(1 2))

  #t)


(parametrise ((check-test-name	'conditionals))

  (check (xor) => #f)
  (check (xor (number? 1)) => #T)
  (check (xor (null? 1)) => #f)
  (check (xor (string->symbol "foo")) => 'foo)
  (check (xor (string? "a") (symbol? 1)) => #T)
  (check (xor (string? 1) (symbol? 'a)) => #T)
  (check (xor (string? 1) (symbol? 2)) => #f)
  (check (xor (pair? '(a)) (list? '(b))) => #f)
  (check (xor (- 42) (not 42)) => -42)
  (check (xor (null? 1) (/ 42)) => 1/42)
  (check (xor (integer? 1.2) (positive? -2) (exact? 3)) => #T)
  (check (xor (integer? 1.2) (positive? 2) (exact? 3.4)) => #T)
  (check (xor (integer? 1) (positive? -2) (exact? 3.4)) => #T)
  (check (xor (integer? 1.2) (positive? -2) (exact? 3.4)) => #f)
  (check (xor (integer? 1.2) (positive? 2) (exact? 3)) => #f)
  (check (xor (integer? 1) (positive? -2) (exact? 3)) => #f)
  (check (xor (integer? 1) (positive? 2) (exact? 3.4)) => #f)
  (check (xor (integer? 1) (positive? 2) (exact? 3)) => #f)
  (check (xor "foo" (not 'foo) (eq? 'a 'b)) => "foo")
  (check (xor (not 'foo) (+ 1 2) (eq? 'a 'b)) => 3)
  (check (xor (not 'foo) (eq? 'a 'b) (- 1 2)) => -1)
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) #f)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #f)
		(begin (set! x (cons 'd x)) #f))
      => #f)
    (check x => '(d c b a)))
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) 'R)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #f)
		(begin (set! x (cons 'd x)) #f))
      => 'R)
    (check x => '(d c b a)))
  (let ((x '()))
    (check (xor (begin (set! x (cons 'a x)) #T)
		(begin (set! x (cons 'b x)) #f)
		(begin (set! x (cons 'c x)) #T)
		(begin (set! x (cons 'd x)) #f))
      => #f)
    (check x => '(c b a)))
  (let-syntax ((macro
		   (let ((count 0))
		     (lambda (stx)
		       (syntax-case stx ()
			 ((_) (begin (set! count (+ 1 count)) #''foo))
			 ((_ _) count))))))
    (check (xor #f (macro) #f) => 'foo)
    (check (macro 'count) => 1))

  #t)


;;;; done

(check-report)

;;; end of file

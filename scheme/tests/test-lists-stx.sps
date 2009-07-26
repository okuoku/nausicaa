;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (lists stx)
;;;Date: Mon Dec 29, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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
  (only (lists)
	circular-list xcons not-pair?)
  (lists stx)
  (rnrs mutable-pairs))

(check-set-mode! 'report-failed)

(define numbers '(0 1 2 3 4 5 6 7 8 9))
(display "*** testing lists syntax\n")


(parameterise ((check-test-name 'constructors))

  (check
      (make-list/stx 4 'c)
    => '(c c c c))

  (check
      (make-list/stx 0)
    => '())

  (check
      (make-list/stx 0 #f)
    => '())

;;; --------------------------------------------------------------------

  (check
      (list-tabulate/stx 4 (lambda (i)
			     (cons i 'a)))
    => '((0 . a)
	 (1 . a)
	 (2 . a)
	 (3 . a)))

  (check
      (list-tabulate/stx 1 (lambda (i)
			     (cons i 'a)))
    => '((0 . a)))

  (check
      (list-tabulate/stx 0 (lambda (i)
			     (cons i 'a)))
    => '())

;;; --------------------------------------------------------------------

  (check
      (list-tabulate/reverse/stx 4 (lambda (i)
				     (cons i 'a)))
    => '((0 . a)
	 (1 . a)
	 (2 . a)
	 (3 . a)))

  (check
      (list-tabulate/reverse/stx 1 (lambda (i)
				     (cons i 'a)))
    => '((0 . a)))

  (check
      (list-tabulate/reverse/stx 0 (lambda (i)
				     (cons i 'a)))
    => '())

;;; --------------------------------------------------------------------

  (check
      (list-copy/stx numbers)
    => numbers)

;;; --------------------------------------------------------------------

  (let ((ell '(1 2 3 4)))
    (check
	(tree-copy/stx ell)
      => ell))

  (let ((ell '()))
    (check
	(tree-copy/stx ell)
      => ell))

  (let ((ell '(1 (2 (3 4) 5 6) 7 8 (9 10))))
    (check
	(tree-copy/stx ell)
      => ell))

;;; --------------------------------------------------------------------

  (check
      (iota/stx 5 10)
    => '(10 11 12 13 14))

  (check
      (iota/stx 5)
    => '(0 1 2 3 4))

  (check
      (iota/stx 5 10 5)
    => '(10 15 20 25 30))

  (guard (exc (else #f))
    (check
	(iota/stx -5 10 5)
      => '()))

;;; --------------------------------------------------------------------

  (check
      (let ((ell (circular-list/stx 1 2)))
	(list (car ell)
	      (cadr ell)
	      (caddr ell)
	      (cadddr ell)))
    => '(1 2 1 2))

  (check
      (let ((ell (list->clist!/stx '(1 2 3))))
	(list (car ell)
	      (cadr ell)
	      (caddr ell)
	      (cadddr ell)
	      (cadddr (cdr ell))))
    => '(1 2 3 1 2))

  )


(parameterise ((check-test-name 'predicates))

  (check (and-null?/stx)		=> #t)
  (check (and-null?/stx '())		=> #t)
  (check (and-null?/stx '() '())	=> #t)
  (check (and-null?/stx '() '() '())	=> #t)

  (check (and-null?/stx '(1))		=> #f)
  (check (and-null?/stx '(1) '(1) '(1))	=> #f)
  (check (and-null?/stx '()  '()  '(1))	=> #f)

  (check (or-null?/stx)			=> #f)
  (check (or-null?/stx '())		=> #t)
  (check (or-null?/stx '() '())		=> #t)
  (check (or-null?/stx '() '() '())	=> #t)

  (check (or-null?/stx '(1))		=> #f)
  (check (or-null?/stx '(1) '(1) '(1))	=> #f)
  (check (or-null?/stx '()  '()  '(1))	=> #t)

  (let-syntax ((check-values	(syntax-rules ()
				  ((_ ?expr ?expected)
				   (check (receive (a o) ?expr (list a o)) => ?expected)))))
    (check-values (and/or-null?/stx)			 '(#t #f))
    (check-values (and/or-null?/stx '())		 '(#t #t))
    (check-values (and/or-null?/stx '() '())		 '(#t #t))
    (check-values (and/or-null?/stx '() '() '())	 '(#t #t))

    (check-values (and/or-null?/stx '(1))		 '(#f #f))
    (check-values (and/or-null?/stx '(1) '(1) '(1))	 '(#f #f))
    (check-values (and/or-null?/stx '()  '()  '(1))	 '(#f #t)))

  )


(parameterise ((check-test-name 'selectors))

  (check (fifth/stx numbers)	=> 4)
  (check (sixth/stx numbers)	=> 5)
  (check (seventh/stx numbers)	=> 6)
  (check (eighth/stx numbers)	=> 7)
  (check (ninth/stx numbers)	=> 8)
  (check (tenth/stx numbers)	=> 9)

;;; --------------------------------------------------------------------

  (check
      (take/stx numbers 5)
    => '(0 1 2 3 4))

  (check
      (take/stx numbers 0)
    => '())

  (check
      (take/stx '() 0)
    => '())

  (check
      (take/stx numbers 10)
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (drop/stx numbers 5)
    => '(5 6 7 8 9))

  (check
      (drop/stx numbers 0)
    => numbers)

  (check
      (drop/stx '() 0)
    => '())

  (check
      (drop/stx numbers 10)
    => '())

;;; --------------------------------------------------------------------

  (check
      (take-right/stx numbers 5)
    => '(5 6 7 8 9))

  (check
      (take-right/stx numbers 0)
    => '())

  (check
      (take-right/stx '() 0)
    => '())

  (check
      (take-right/stx numbers 10)
    => numbers)

;;; --------------------------------------------------------------------

  (check
      (drop-right/stx numbers 5)
    => '(0 1 2 3 4))

  (check
      (drop-right/stx numbers 0)
    => numbers)

  (check
      (drop-right/stx '() 0)
    => '())

  (check
      (drop-right/stx numbers 10)
    => '())

;;; --------------------------------------------------------------------

  (check
      (take!/stx '(1 3 5) 2)
    => '(1 3))

  (check
      (guard (exc (else #t))
	(take!/stx '() 2))
    => #t)

  (check
      (drop-right!/stx '(1 3 5) 1)
    => '(1 3))

  (check
      (guard (exc (else #t))
	(drop-right!/stx '() 1))
    => #t)

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (split-at/stx numbers 5))
	list)
    => '((0 1 2 3 4)
	 (5 6 7 8 9)))

;;; --------------------------------------------------------------------

  (check
      (last/stx numbers)
    => 9)

  (check
      (last/stx '(9))
    => 9)

;;; This raises an error.
  ;;
  ;; (check
  ;;     (last/stx '())
  ;;   => '())

;;; --------------------------------------------------------------------

  (check
      (last-pair/stx numbers)
    => '(9))

  (check
      (last-pair/stx '(9))
    => '(9))

;;; The empty list is not a pair, so the following raises an error.
  ;;
  ;; (check
  ;;     (last-pair/stx '())
  ;;   => '())

  )


(parameterise ((check-test-name 'miscellaneous))

  (check
      (concatenate/stx '())
    => '())

  (check
      (concatenate/stx '(()))
    => '())

  (check
      (concatenate/stx '(() ()))
    => '())

  (check
      (concatenate/stx '(() () ()))
    => '())

  (check
      (concatenate/stx '((x)))
    => '(x))

  (check
      (concatenate/stx '((x) (y)))
    => '(x y))

  (check
      (concatenate/stx '((x) (y) (z)))
    => '(x y z))

  (check
      (concatenate/stx '((a)
			 (b c d)))
    => '(a b c d))

  (check
      (concatenate/stx '((a b)
			 (c d)))
    => '(a b c d))

  (check
      (concatenate/stx '((a b)
			 (c d)
			 (e f)))
    => '(a b c d e f))

  (check
      (concatenate/stx '((a b c d e f g)
			 (h i)
			 (l m n o)))
    => '(a b c d e f g h i l m n o))

  (check
      (concatenate/stx '((a (b)) ((c))))
    => '(a (b) (c)))

  (check
      (concatenate/stx '((a b) (c . d)))
    => '(a b c . d))

  (check
      (concatenate/stx '(() (a)))
    => '(a))

  (check
      (concatenate/stx '((x y)))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (concatenate!/stx '())
    => '())

  (check
      (concatenate!/stx '(()))
    => '())

  (check
      (concatenate!/stx '(() ()))
    => '())

  (check
      (concatenate!/stx '(() () ()))
    => '())

  (check
      (concatenate!/stx '((x)))
    => '(x))

  (check
      (concatenate!/stx '((x) (y)))
    => '(x y))

  (check
      (concatenate!/stx '((x) (y) (z)))
    => '(x y z))

  (check
      (concatenate!/stx '((a)
			  (b c d)))
    => '(a b c d))

  (check
      (concatenate!/stx '((a b)
			  (c d)))
    => '(a b c d))

  (check
      (concatenate!/stx '((a b)
			  (c d)
			  (e f)))
    => '(a b c d e f))

  (check
      (concatenate!/stx '((a b c d e f g)
			  (h i)
			  (l m n o)))
    => '(a b c d e f g h i l m n o))

  (check
      (concatenate!/stx '((a (b)) ((c))))
    => '(a (b) (c)))

  (check
      (concatenate!/stx '((a b) (c . d)))
    => '(a b c . d))

  (check
      (concatenate!/stx '(() (a)))
    => '(a))

  (check
      (concatenate!/stx '((x y)))
    => '(x y))

;;; --------------------------------------------------------------------

  (check
      (append-reverse/stx '() '())
    => '())

  (check
      (append-reverse/stx '(x) '(y))
    => '(x y))

  (check
      (append-reverse/stx '(1 2 3) '(4 5 6))
    => '(3 2 1 4 5 6))

  (check
      (append-reverse/stx '(a) '(b c d))
    => '(a b c d))

  (check
      (append-reverse/stx '(a (b)) '((c)))
    => '((b) a (c)))

  (check
      (append-reverse/stx '(a) '())
    => '(a))

;;; --------------------------------------------------------------------

  (check
      (append-reverse!/stx '() '())
    => '())

  (check
      (append-reverse!/stx '(x) '(y))
    => '(x y))

  (check
      (append-reverse!/stx '(1 2 3) '(4 5 6))
    => '(3 2 1 4 5 6))

  (check
      (append-reverse!/stx '(a) '(b c d))
    => '(a b c d))

  (check
      (append-reverse!/stx '(a (b)) '((c)))
    => '((b) a (c)))

  (check
      (append-reverse!/stx '(a) '())
    => '(a))

;;; --------------------------------------------------------------------

  (check
      (zip/stx '(one two three)
	       '(1 2 3)
	       '(odd even odd even odd even odd even))
    => '((one 1 odd) (two 2 even) (three 3 odd)))

  (check
      (zip/stx '(1 2 3))
    => '((1) (2) (3)))

  (check
      (zip/stx '(3 1 4 1)
	       (circular-list #f #t))
    => '((3 #f)
	 (1 #t)
	 (4 #f)
	 (1 #t)))

;;; --------------------------------------------------------------------

  (check
      (unzip1/stx '((1)))
    => '(1))

  (check
      (unzip1/stx '((1)
		    (2)))
    => '(1 2))

  (check
      (unzip1/stx '((1)
		    (2)
		    (3)))
    => '(1 2 3))

  (check
      (unzip1/stx '((1 one)
		    (2 two)
		    (3 three)))
    => '(1 2 3))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (unzip2/stx '((1 one))))
	list)
    => '((1)
	 (one)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip2/stx '((1 one)
			  (2 two))))
	list)
    => '((1 2)
	 (one two)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip2/stx '((1 one)
			  (2 two)
			  (3 three))))
	list)
    => '((1 2 3)
	 (one two three)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (unzip3/stx '((1 10 100)
			  (2 20 200)
			  (3 30 300))))
	list)
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip4/stx '((1 10 100 1000)
			  (2 20 200 2000)
			  (3 30 300 3000))))
	list)
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)
	 (1000 2000 3000)))

  (check
      (call-with-values
	  (lambda ()
	    (unzip5/stx '((1 10 100 1000 10000)
			  (2 20 200 2000 20000)
			  (3 30 300 3000 30000))))
	list)
    => '((1 2 3)
	 (10 20 30)
	 (100 200 300)
	 (1000 2000 3000)
	 (10000 20000 30000)))


;;; --------------------------------------------------------------------

  (check
      (count/stx even? numbers)
    => 5)

  (check
      (count/stx even? '(1))
    => 0)

  (check
      (count/stx even? '(2))
    => 1)

  (check
      (count/stx even? '())
    => 0)

  )


(parameterise ((check-test-name 'left-folding-syntax))

  (check
      (fold/stx + 0 numbers)
    => 45)

  (check
      (fold/stx cons '() numbers)
    => '(9 8 7 6 5 4 3 2 1 0))

  (check
      (fold/stx cons '(4 5 6) '(3 2 1))
    => '(1 2 3 4 5 6))

  (check
      (fold/stx cons '(4 5 6) '())
    => '(4 5 6))

  (check
      (fold/stx cons '(4 5 6) '(3))
    => '(3 4 5 6))

  (check
      (fold/stx (lambda (x count)
		  (if (symbol? x)
		      (+ count 1)
		    count))
		0
		'(a 1 b 2 c 3))
    => 3)

  (check
      (fold/stx (lambda (s len)
		  (max len (string-length s)))
		0
		'("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold/stx cons* '()
		'(a b c)
		'(1 2 3 4 5))
    => '(c 3 b 2 a 1))

  (check
      (fold/stx cons* '()
		'(a)
		'(1))
    => '(a 1))

  (check
      (fold/stx (lambda (a b c knil)
		  (cons (list a b c)
			knil))
		'()
		'(1 2 3)
		'(10 20 30)
		'(100 200 300))
    => '((3 30 300)
	 (2 20 200)
	 (1 10 100)))

  (check
      (fold/stx (lambda (a b c knil)
		  (cons (list a b c)
			knil))
		'()
		'(1 2 3)
		'(10 20)
		'(100 200 300 400))
    => '((2 20 200)
	 (1 10 100)))

;;; --------------------------------------------------------------------

  (check
      (fold-left/stx + 0 numbers)
    => 45)

  (check
      (fold-left/stx xcons '() numbers)
    => '(9 8 7 6 5 4 3 2 1 0))

  (check
      (fold-left/stx xcons '(4 5 6) '(3 2 1))
    => '(1 2 3 4 5 6))

  (check
      (fold-left/stx xcons '(4 5 6) '())
    => '(4 5 6))

  (check
      (fold-left/stx xcons '(4 5 6) '(3))
    => '(3 4 5 6))

  (check
      (fold-left/stx (lambda (count x)
			(if (symbol? x)
			    (+ count 1)
			  count))
		      0
		      '(a 1 b 2 c 3))
    => 3)

  (check
      (fold-left/stx (lambda (len s)
			(max len (string-length s)))
		      0
		      '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (guard (exc ((assertion-violation? exc) (condition-message exc)))
	(fold-left/stx (lambda (knil a b c)
			 (cons (list a b c)
			       knil))
		       '()
		       '(1 2 3)
		       '(10 20)
		       '(100 200 300 400)))
    => "expected lists of equal length")

;;; --------------------------------------------------------------------

  (check
      (fold-left*/stx + 0 numbers)
    => 45)

  (check
      (fold-left*/stx xcons '() numbers)
    => '(9 8 7 6 5 4 3 2 1 0))

  (check
      (fold-left*/stx xcons '(4 5 6) '(3 2 1))
    => '(1 2 3 4 5 6))

  (check
      (fold-left*/stx xcons '(4 5 6) '())
    => '(4 5 6))

  (check
      (fold-left*/stx xcons '(4 5 6) '(3))
    => '(3 4 5 6))

  (check
      (fold-left*/stx (lambda (count x)
			(if (symbol? x)
			    (+ count 1)
			  count))
		      0
		      '(a 1 b 2 c 3))
    => 3)

  (check
      (fold-left*/stx (lambda (len s)
			(max len (string-length s)))
		      0
		      '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold-left*/stx (lambda (knil a b c)
			(cons (list a b c)
			      knil))
		      '()
		      '(1 2 3)
		      '(10 20 30)
		      '(100 200 300))
    => '((3 30 300)
	 (2 20 200)
	 (1 10 100)))

  (check
      (fold-left*/stx (lambda (knil a b c)
			(cons (list a b c)
			      knil))
		      '()
		      '(1 2 3)
		      '(10 20)
		      '(100 200 300 400))
    => '((2 20 200)
	 (1 10 100)))

  )


(parameterise ((check-test-name 'right-folding-syntax))

  (check
      (fold*/stx cons '() '(1 2 3))
    => '(1 2 3))

  (check
      (fold*/stx cons '() numbers)
    => numbers)

  (check
      (fold*/stx + 0 numbers)
    => 45)

  (check
      (fold*/stx cons '(4 5 6) '(1 2 3))
    => '(1 2 3 4 5 6))

  (check
      (fold*/stx (lambda (x count)
		   (if (symbol? x)
		       (+ count 1)
		     count))
		 0
		 '(a 1 b 2 c 3))
    => 3)

  (check
      (fold*/stx (lambda (s len)
		   (max len (string-length s)))
		 0
		 '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold*/stx (lambda (x l)
		   (if (even? x)
		       (cons x l)
		     l))
		 '()
		 '(0 1 2 3 4 5 6 7 8 9))
    => '(0 2 4 6 8))

  (check
      (fold*/stx cons* '()
		 '(a b c)
		 '(1 2 3 4 5))
    => '(a 1 b 2 c 3))

  (check
      (fold*/stx cons* '()
		 '(a)
		 '(1))
    => '(a 1))

  (check
      (fold*/stx (lambda (a b c knil)
		   (cons (list a b c)
			 knil))
		 '()
		 '(1 2 3)
		 '(10 20 30)
		 '(100 200 300))
    => '((1 10 100)
	 (2 20 200)
	 (3 30 300)))

  (check
      (fold*/stx (lambda (a b c knil)
		   (cons (list a b c)
			 knil))
		 '()
		 '(1 2 3)
		 '(10 20)
		 '(100 200 300 400))
    => '((1 10 100)
	 (2 20 200)))

;;; --------------------------------------------------------------------

  (check
      (fold-right/stx cons '() '(1 2 3))
    => '(1 2 3))

  (check
      (fold-right/stx cons '(1 2 3) '())
    => '(1 2 3))

  (check
      (fold-right/stx cons '(1 2 3) '(9))
    => '(9 1 2 3))

  (check
      (fold-right/stx cons '() numbers)
    => numbers)

  (check
      (fold-right/stx + 0 numbers)
    => 45)

  (check
      (fold-right/stx cons '(4 5 6) '(1 2 3))
    => '(1 2 3 4 5 6))

  (check
      (fold-right/stx (lambda (x count)
			(if (symbol? x)
			    (+ count 1)
			  count))
		      0
		      '(a 1 b 2 c 3))
    => 3)

  (check
      (fold-right/stx (lambda (s len)
			(max len (string-length s)))
		      0
		      '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold-right/stx (lambda (x l)
			(if (even? x)
			    (cons x l)
			  l))
		      '()
		      '(0 1 2 3 4 5 6 7 8 9))
    => '(0 2 4 6 8))

  (check
      (fold-right/stx cons* '()
		      '(a b c)
		      '(1 2 3))
    => '(a 1 b 2 c 3))

  (check
      (fold-right/stx cons* '()
		      '(a)
		      '(1))
    => '(a 1))

  (check
      (fold-right/stx (lambda (a b c knil)
			(cons (list a b c)
			      knil))
		      '()
		      '(1 2 3)
		      '(10 20 30)
		      '(100 200 300))
    => '((1 10 100)
	 (2 20 200)
	 (3 30 300)))

  (check
      (guard (exc ((assertion-violation? exc) (condition-message exc)))
	(fold-right/stx (lambda (a b c knil)
			  (cons (list a b c)
				knil))
			'()
			'(1 2 3)
			'(10 20)
			'(100 200 300 400)))
    => "expected lists of equal length")

;;; --------------------------------------------------------------------

  (check
      (fold-right*/stx cons '() '(1 2 3))
    => '(1 2 3))

  (check
      (fold-right*/stx cons '(1 2 3) '())
    => '(1 2 3))

  (check
      (fold-right*/stx cons '(1 2 3) '(9))
    => '(9 1 2 3))

  (check
      (fold-right*/stx cons '() numbers)
    => numbers)

  (check
      (fold-right*/stx + 0 numbers)
    => 45)

  (check
      (fold-right*/stx cons '(4 5 6) '(1 2 3))
    => '(1 2 3 4 5 6))

  (check
      (fold-right*/stx (lambda (x count)
			 (if (symbol? x)
			     (+ count 1)
			   count))
		       0
		       '(a 1 b 2 c 3))
    => 3)

  (check
      (fold-right*/stx (lambda (s len)
			 (max len (string-length s)))
		       0
		       '("ciao" "hello" "salut" "hola"))
    => 5)

  (check
      (fold-right*/stx (lambda (x l)
			 (if (even? x)
			     (cons x l)
			   l))
		       '()
		       '(0 1 2 3 4 5 6 7 8 9))
    => '(0 2 4 6 8))

  (check
      (fold-right*/stx cons* '()
		       '(a b c)
		       '(1 2 3))
    => '(a 1 b 2 c 3))

  (check
      (fold-right*/stx cons* '()
		       '(a)
		       '(1))
    => '(a 1))

  (check
      (fold-right*/stx (lambda (a b c knil)
			 (cons (list a b c)
			       knil))
		       '()
		       '(1 2 3)
		       '(10 20 30)
		       '(100 200 300))
    => '((1 10 100)
	 (2 20 200)
	 (3 30 300)))

  (check
      (fold-right*/stx (lambda (a b c knil)
			 (cons (list a b c)
			       knil))
		       '()
		       '(1 2 3)
		       '(10 20)
		       '(100 200 300 400))
    => '((1 10 100)
	 (2 20 200)))

  )


(parameterise ((check-test-name 'reducing))

  (check
      (reduce/stx + 0 numbers)
    => 45)

  (check
      (reduce/stx + 0 '())
    => 0)

  (check
      (reduce/stx max 0 '(1 2 3 4 5))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (reduce*/stx + 0 numbers)
    => 45)

  (check
      (reduce*/stx + 0 '())
    => 0)

  (check
      (reduce*/stx max 0 '(1 2 3 4 5))
    => 5)

  (check
      (reduce*/stx append
		   '()
		   '((1 2 3)
		     (4 5)
		     (6 7 8 9)
		     (0)))
    => '(1 2 3 4 5 6 7 8 9 0))

  )


(parameterise ((check-test-name 'unfolding))

  (check
      (unfold/stx (lambda (x) (< 5 x))
	      (lambda (x) (* x x))
	      (lambda (x) (+ x 1))
	      1)
    => '(1 4 9 16 25))

  (check
      (unfold/stx (lambda (x) (< 5 x))
	      (lambda (x) (* x x))
	      (lambda (x) (+ x 1))
	      1
	      (lambda (x) (- x)))
    => '(1 4 9 16 25 . -6))

  (check
      (unfold/stx (lambda (x) #t)
	      (lambda (x) (* x x))
	      (lambda (x) (+ x 1))
	      1
	      (lambda (x) (- x)))
    => -1)

  (check
      (unfold/stx null? car cdr numbers)
    => numbers)

  (check
      (unfold/stx not-pair? car cdr '(1 2 3 4 . 5) values)
    => '(1 2 3 4 . 5))

  (check
      (unfold/stx null? car cdr '(1 2 3) (lambda (x) '(4 5 6)))
    => '(1 2 3 4 5 6))

;;; --------------------------------------------------------------------

  (check
      (unfold-right/stx zero?
		    (lambda (x) (* x x))
		    (lambda (x) (- x 1))
		    5)
    => '(1 4 9 16 25))

  (check
      (unfold-right/stx null? car cdr '(1 2 3 4 5))
    => '(5 4 3 2 1))

  (check
      (unfold-right/stx null? car cdr '(3 2 1) '(4 5 6))
    => '(1 2 3 4 5 6))

  )


(parameterise ((check-test-name 'mapping))

  (check
      (map*/stx - '())
    => '())

  (check
      (map*/stx - '() '())
    => '())

  (check
      (map*/stx - '() '() '())
    => '())

  (check
      (map*/stx - '() '() '() '())
    => '())

  (check
      (map*/stx - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (map*/stx + '(1 2 3))
    => '(1 2 3))

  (check
      (map*/stx +
		'(1 2 3)
		'(10 20 30))
    => '(11 22 33))

  (check
      (map*/stx +
		'(1 2 3)
		'(10 20 30)
		'(100 200 300))
    => '(111 222 333))

  (check
      (map*/stx +
		'(1 2 3)
		'(10 20)
		'(100 200 300))
    => '(111 222))

  (check
      (map*/stx +
		'(1 2)
		'(10 20 30)
		'(100 200 300))
    => '(111 222))

  (check
      (map*/stx +
		'(1 2 3)
		'(10 20 30)
		'(100 200))
    => '(111 222))

  (check
      (map*/stx +
		'()
		'(10 20 30)
		'(100 200 300))
    => '())

  (check
      (map*/stx +
		'(1 2 3)
		'()
		'(100 200 300))
    => '())

  (check
      (map*/stx +
		'(1 2 3)
		'(10 20 30)
		'())
    => '())

  (check
      (map*/stx +
		'(3 1 4 1)
		(circular-list 1 0))
    => '(4 1 5 1))

;;; --------------------------------------------------------------------

  (check
      (map-in-order/stx - '())
    => '())

  (check
      (map-in-order/stx - '() '())
    => '())

  (check
      (map-in-order/stx - '() '() '())
    => '())

  (check
      (map-in-order/stx - '() '() '() '())
    => '())

  (check
      (map-in-order/stx - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (map-in-order/stx + '(1 2 3))
    => '(1 2 3))

  (check
      (map-in-order/stx +
			'(1 2 3)
			'(10 20 30))
    => '(11 22 33))

  (check
      (map-in-order/stx +
			'(1 2 3)
			'(10 20 30)
			'(100 200 300))
    => '(111 222 333))

  (check
      (guard (exc (else (condition-message exc)))
	(map-in-order/stx +
			  '(1 2 3)
			  '(10 20)
			  '(100 200 300)))
    => "expected lists of equal length")

  (check
      (guard (exc (else (condition-message exc)))
	(map-in-order/stx +
			  '(1 2)
			  '(10 20 30)
			  '(100 200 300)))
    => "expected lists of equal length")

  (check
      (guard (exc (else (condition-message exc)))
	(map-in-order/stx +
			  '(1 2 3)
			  '(10 20 30)
			  '(100 200)))
    => "expected lists of equal length")

  (check
      (guard (exc (else (condition-message exc)))
	(map-in-order/stx +
			  '()
			  '(10 20 30)
			  '(100 200 300)))
    => "expected lists of equal length")

  (check
      (guard (exc (else (condition-message exc)))
	(map-in-order/stx +
			  '(1 2 3)
			  '()
			  '(100 200 300)))
    => "expected lists of equal length")

  (check
      (guard (exc (else (condition-message exc)))
	(map-in-order/stx +
			  '(1 2 3)
			  '(10 20 30)
			  '()))
    => "expected lists of equal length")

  (check
      (guard (exc (else (condition-message exc)))
	(map-in-order/stx +
			  '(3 1 4 1)
			  (circular-list 1 0)))
    => "expected lists of equal length")

;;; --------------------------------------------------------------------

    (check
        (map-in-order*/stx - '())
      => '())

    (check
        (map-in-order*/stx - '() '())
      => '())

    (check
        (map-in-order*/stx - '() '() '())
      => '())

    (check
        (map-in-order*/stx - '() '() '() '())
      => '())

    (check
        (map-in-order*/stx - numbers)
      => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

    (check
        (map-in-order*/stx + '(1 2 3))
      => '(1 2 3))

    (check
        (map-in-order*/stx +
  			 '(1 2 3)
  			 '(10 20 30))
      => '(11 22 33))

    (check
        (map-in-order*/stx +
  			 '(1 2 3)
  			 '(10 20 30)
  			 '(100 200 300))
      => '(111 222 333))

    (check
        (map-in-order*/stx +
  			 '(1 2 3)
  			 '(10 20)
  			 '(100 200 300))
      => '(111 222))

    (check
        (map-in-order*/stx +
  			 '(1 2)
  			 '(10 20 30)
  			 '(100 200 300))
      => '(111 222))

    (check
        (map-in-order*/stx +
  			 '(1 2 3)
  			 '(10 20 30)
  			 '(100 200))
      => '(111 222))

    (check
        (map-in-order*/stx +
  			 '()
  			 '(10 20 30)
  			 '(100 200 300))
      => '())

    (check
        (map-in-order*/stx +
  			 '(1 2 3)
  			 '()
  			 '(100 200 300))
      => '())

    (check
        (map-in-order*/stx +
  			 '(1 2 3)
  			 '(10 20 30)
  			 '())
      => '())

    (check
        (map-in-order*/stx +
  			 '(3 1 4 1)
  			 (circular-list 1 0))
      => '(4 1 5 1))

;;; --------------------------------------------------------------------

  (check
      (let ((r 0))
	(for-each*/stx
	 (lambda (e)
	   (set! r (+ e r)))
	 '())
	r)
    => 0)

  (check
      (let ((r 0))
	(for-each*/stx
	 (lambda (e1 e2)
	   (set! r (+ e1 e2 r)))
	 '() '())
	r)
    => 0)

  (check
      (let ((r 0))
	(for-each*/stx
	 (lambda (e1 e2 e3)
	   (set! r (+ e1 e2 e3 r)))
	 '() '() '())
	r)
    => 0)

  (check
      (let ((r '(0 0)))
	(for-each*/stx
	 (lambda (e1 e2)
	   (set! r (list (+ e1 (car r))
			 (+ e2 (cadr r)))))
	 '(1 10 100)
	 '(2 20 200))
	r)
    => '(111 222))


    (check
        (let ((r '(0 0 0)))
  	(for-each*/stx
  	 (lambda (e1 e2 e3)
  	   (set! r (list (+ e1 (car r))
  			 (+ e2 (cadr r))
  			 (+ e3 (caddr r)))))
  	 '(1 10 100)
  	 '(2 20 200)
  	 '(3 30 300))
  	r)
      => '(111 222 333))

    (check
        (let ((r '(0 0 0)))
  	(for-each*/stx
  	 (lambda (e1 e2 e3)
  	   (set! r (list (+ e1 (car r))
  			 (+ e2 (cadr r))
  			 (+ e3 (caddr r)))))
  	 '(1 10 100)
  	 '(2 20 200)
  	 (circular-list 3 30 300))
  	r)
      => '(111 222 333))

;;; --------------------------------------------------------------------

  (check
      (map!/stx - '())
    => '())

  (check
      (map!/stx - (list-copy/stx numbers))
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (map!/stx + '(1 2 3))
    => '(1 2 3))

  (check
      (map!/stx - '() '())
    => '())

  (check
      (map!/stx - '() '() '())
    => '())

  (check
      (map!/stx - '() '() '() '())
    => '())

  (check
      (map!/stx +
		'(1 2 3)
		'(10 20 30))
    => '(11 22 33))

  (check
      (map!/stx +
		'(1 2 3)
		'(10 20 30)
		'(100 200 300))
    => '(111 222 333))

;;; Only the first list argument can be shorter!!!
  (check
      (map!/stx +
		'(1 2)
		'(10 20 30)
		'(100 200 300))
    => '(111 222))

  (check
      (map!/stx +
		'()
		'(10 20 30)
		'(100 200 300))
    => '())

  (check
      (map!/stx +
		'(3 1 4 1)
		(circular-list 1 0))
    => '(4 1 5 1))

;;; --------------------------------------------------------------------

  (check
      (map*!/stx - '())
    => '())

  (check
      (map*!/stx - (list-copy/stx numbers))
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (map*!/stx + '(1 2 3))
    => '(1 2 3))

  (check
      (map*!/stx - '() '())
    => '())

  (check
      (map*!/stx - '() '() '())
    => '())

  (check
      (map*!/stx - '() '() '() '())
    => '())

  (check
      (map*!/stx +
		 '(1 2 3)
		 '(10 20 30))
    => '(11 22 33))

  (check
      (map*!/stx +
		 '(1 2 3)
		 '(10 20 30)
		 '(100 200 300))
    => '(111 222 333))

;;; Only the first list argument can be shorter!!!
  (check
      (map*!/stx +
		 '(1 2)
		 '(10 20 30)
		 '(100 200 300))
    => '(111 222))

  (check
      (map*!/stx +
		 '()
		 '(10 20 30)
		 '(100 200 300))
    => '())

  (check
      (map*!/stx +
		 '(3 1 4 1)
		 (circular-list 1 0))
    => '(4 1 5 1))

;;; --------------------------------------------------------------------

  (check
      (filter-map/stx
       (lambda (x)
	 (and (number? x)
	      (* x x)))
       '(a 1 b 3 c 7))
    => '(1 9 49))

  (check
      (filter-map/stx - '())
    => '())

  (check
      (filter-map/stx - '() '())
    => '())

  (check
      (filter-map/stx - '() '() '())
    => '())

  (check
      (filter-map/stx - '() '() '() '())
    => '())

  (check
      (filter-map/stx - numbers)
    => '(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))

  (check
      (filter-map/stx + '(1 2 3))
    => '(1 2 3))

  (check
      (filter-map/stx +
		      '(1 2 3)
		      '(10 20 30))
    => '(11 22 33))

  (check
      (filter-map/stx +
		      '(1 2 3)
		      '(10 20 30)
		      '(100 200 300))
    => '(111 222 333))

  (check
      (filter-map/stx +
		      '(1 2 3)
		      '(10 20)
		      '(100 200 300))
    => '(111 222))

  (check
      (filter-map/stx +
		      '(1 2)
		      '(10 20 30)
		      '(100 200 300))
    => '(111 222))

  (check
      (filter-map/stx +
		      '(1 2 3)
		      '(10 20 30)
		      '(100 200))
    => '(111 222))

  (check
      (filter-map/stx +
		      '()
		      '(10 20 30)
		      '(100 200 300))
    => '())

  (check
      (filter-map/stx +
		      '(1 2 3)
		      '()
		      '(100 200 300))
    => '())

  (check
      (filter-map/stx +
		      '(1 2 3)
		      '(10 20 30)
		      '())
    => '())

  (check
      (filter-map/stx +
		      '(3 1 4 1)
		      (circular-list 1 0))
    => '(4 1 5 1))

  )


#;(parameterise ((check-test-name 'filtering))

  (check
      (filter even? '())
    => '())

  (check
      (filter even? '(1))
    => '())

  (check
      (filter even? '(2))
    => '(2))

  (check
      (filter even? numbers)
    => '(0 2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (filter! even? '())
    => '())

  (check
      (filter! even? '(1))
    => '())

  (check
      (filter! even? '(2))
    => '(2))

  (check
      (filter! even? (list-copy numbers))
    => '(0 2 4 6 8))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '(1 3)))
	list)
    => '(() (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? '(2 4)))
	list)
    => '((2 4) ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition even? numbers))
	list)
    => '((0 2 4 6 8)
	 (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(1 3)))
	list)
    => '(() (1 3)))

  (check
      (call-with-values
	  (lambda ()
	    (partition! even? '(2 4)))
	list)
    => '((2 4) ()))


  (check
      (call-with-values
	  (lambda ()
	    (partition! even? (list-copy numbers)))
	list)
    => '((0 2 4 6 8)
	 (1 3 5 7 9)))

;;; --------------------------------------------------------------------

  (check
      (remove 8 numbers)
    => '(0 1 2 3 4 5 6 7 9))

  (check
      (remove 8 '(1 2 3))
    => '(1 2 3))

  (check
      (remove 8 '(1))
    => '(1))

  (check
      (remove 8 '())
    => '())

;;; --------------------------------------------------------------------

  (check
      (remove* even? '())
    => '())

  (check
      (remove* even? '(1))
    => '(1))

  (check
      (remove* even? '(2))
    => '())

  (check
      (remove* even? numbers)
    => '(1 3 5 7 9))

;;; --------------------------------------------------------------------

  (check
      (remove*! even? '())
    => '())

  (check
      (remove*! even? '(1))
    => '(1))

  (check
      (remove*! even? '(2))
    => '())

  (check
      (remove*! even? (list-copy numbers))
    => '(1 3 5 7 9))

  )


#;(parameterise ((check-test-name 'finding))

  (check
      (find even? '())
    => #f)

  (check
      (find even? '(1))
    => #f)

  (check
      (find even? '(2))
    => 2)

  (check
      (find even? '(1 2 3))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (find-tail even? '())
    => #f)

  (check
      (find-tail even? '(1))
    => #f)

  (check
      (find-tail even? '(2))
    => '(2))

  (check
      (find-tail even? '(1 2 3))
    => '(2 3))

;;; --------------------------------------------------------------------

  (check
      (take-while even? '())
    => '())

  (check
      (take-while even? '(1))
    => '())

  (check
      (take-while even? '(2))
    => '(2))

  (check
      (take-while even? '(2 4 6 1 3))
    => '(2 4 6))

;;; --------------------------------------------------------------------

  (check
      (take-while! even? '())
    => '())

  (check
      (take-while! even? '(1))
    => '())

  (check
      (take-while! even? '(2))
    => '(2))

  (check
      (take-while! even? '(2 4 6 1 3))
    => '(2 4 6))

;;; --------------------------------------------------------------------

  (check
      (drop-while even? '())
    => '())

  (check
      (drop-while even? '(1))
    => '(1))

  (check
      (drop-while even? '(2))
    => '())

  (check
      (drop-while even? '(2 4 6 1 3))
    => '(1 3))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (span even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (span even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda () (span even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda () (span even? '(2 4 6 1 3)))
	list)
    => '((2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (span! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (span! even? '(1)))
	list)
    => '(() (1)))

  (check
      (call-with-values
	  (lambda () (span! even? '(2)))
	list)
    => '((2) ()))

  (check
      (call-with-values
	  (lambda () (span! even? '(2 4 6 1 3)))
	list)
    => '((2 4 6) (1 3)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (break even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (break even? '(1)))
	list)
    => '((1) ()))

  (check
      (call-with-values
	  (lambda () (break even? '(2)))
	list)
    => '(() (2)))

  (check
      (call-with-values
	  (lambda () (break even? '(1 3 2 4 6)))
	list)
    => '((1 3) (2 4 6)))

;;; --------------------------------------------------------------------

  (check
      (call-with-values
	  (lambda () (break! even? '()))
	list)
    => '(() ()))

  (check
      (call-with-values
	  (lambda () (break! even? '(1)))
	list)
    => '((1) ()))

  (check
      (call-with-values
	  (lambda () (break! even? '(2)))
	list)
    => '(() (2)))

  (check
      (call-with-values
	  (lambda () (break! even? '(1 3 2 4 6)))
	list)
    => '((1 3) (2 4 6)))

;;; --------------------------------------------------------------------

  (check
      (any even? '())
    => #f)

  (check
      (any even? '(1))
    => #f)

  (check
      (and (any even? '(2))
	   #t)
    => #t)

  (check
      (and (any even? '(1 2))
	   #t)
    => #t)

  (check
      (and (any even? '(1 3 5 7 2))
	   #t)
    => #t)

  (check
      (any (lambda args
	     (integer? (apply + args)))
	'() '())
    => #f)

  (check
      (any (lambda args
	     (integer? (apply + args)))
	'() '() '())
    => #f)

;;; The following are  false because when a list  is empty the predicate
;;; is not applied at all and the return value is false.
  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1) '() '())
	   #t)
    => #f)
  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '() '(1) '())
	   #t)
    => #f)
  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '() '() '(1))
	   #t)
    => #f)

  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1) '(1.1) '(2))
	   #t)
    => #f)

  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1) '(2) '(2))
	   #t)
    => #t)

  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1 2)
	     '(2 2.2)
	     '(1.1 3))
	   #t)
    => #f)

  (check
      (and (any (lambda args
		  (integer? (apply + args)))
	     '(1 2)
	     '(2 2)
	     '(1.1 3))
	   #t)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (every even? '())
    => #t)

  (check
      (every even? '(1))
    => #f)

  (check
      (and (every even? '(2))
	   #t)
    => #t)

  (check
      (and (every even? '(1 2))
	   #t)
    => #f)

  (check
      (and (every even? '(4 8 10 12))
	   #t)
    => #t)

  (check
      (every (lambda args
	       (integer? (apply + args)))
	'() '())
    => #t)

  (check
      (every (lambda args
	       (integer? (apply + args)))
	'() '() '())
    => #t)

;;; The following are true because when a list is empty the predicate is
;;; not applied at all and the return value is true.
  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1) '() '())
	   #t)
    => #t)
  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '() '(1) '())
	   #t)
    => #t)
  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '() '() '(1))
	   #t)
    => #t)

  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1) '(1.1) '(2))
	   #t)
    => #f)

  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1) '(2) '(2))
	   #t)
    => #t)

  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1 2)
	     '(2 2.2)
	     '(1 3))
	   #t)
    => #f)

  (check
      (and (every (lambda args
		    (integer? (apply + args)))
	     '(1 2)
	     '(2 2)
	     '(1 3))
	   #t)
    => #t)

;;; --------------------------------------------------------------------

  (check
      (list-index even? '())
    => #f)

  (check
      (list-index even? '() '())
    => #f)

  (check
      (list-index even? '() '() '())
    => #f)

  (check
      (list-index even? '(1))
    => #f)

  (check
      (list-index even? '(1 3 5))
    => #f)

  (check
      (list-index even? '(2))
    => 0)

  (check
      (list-index even? '(1 2 3 5))
    => 1)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1 2 3))
    => 0)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1.1 2 3))
    => 1)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1 2 3)
	'(1 2 3))
    => 0)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1.1 2 3)
	'(1 2 3))
    => 1)

  (check
      (list-index (lambda args
		    (integer? (apply + args)))
	'(1 2 3)
	'(1 2 3)
	'(1.1 2.1 3))
    => 2)

;;; --------------------------------------------------------------------

  (check
      (memq 'a '(a b c))
    => '(a b c))

  (check
      (memq 'b '(a b c))
    => '(b c))

  (check
      (memq 'a '(b c d))
    => #f)

  (check
      (memq (list 'a) '(b (a) c))
    => #f)

;;; --------------------------------------------------------------------

  (check
      (member '(a)
	      '(b (a) c))
    => '((a) c))

  (check
      (member* '(a)
	       '(b (a) c))
    => '((a) c))

  (check
      (member* '(a)
	       '(b a c))
    => #f)

  (check
      (member* '(a)
	       '())
    => #f)

  (check
      (member* 10
	       '(1 2 3 11 4 5)
	       (lambda (a b)
		 (= (+ 1 a) b)))
    => '(11 4 5))

;;; --------------------------------------------------------------------

  (check
      (memv 101 '(100 101 102))
    => '(101 102))

  )


#;(parameterise ((check-test-name 'deletion))

  (check
      (delete 8 '())
    => '())

  (check
      (delete 8 '(1))
    => '(1))

  (check
      (delete 8 '(8))
    => '())

  (check
      (delete 8 '(1 2 3))
    => '(1 2 3))

  (check
      (delete 8 '(1 2 8 3 4 5 8 6 7 8))
    => '(1 2 3 4 5 6 7))

  (check
      (delete 8 '() =)
    => '())

  (check
      (delete 8 '(1) =)
    => '(1))

  (check
      (delete 8 '(8) =)
    => '())

  (check
      (delete 8 '(1 2 3) =)
    => '(1 2 3))

  (check
      (delete 8 '(1 2 8 3 4 5 8 6 7 8) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete! 8 '())
    => '())

  (check
      (delete! 8 '(1))
    => '(1))

  (check
      (delete! 8 '(8))
    => '())

  (check
      (delete! 8 '(1 2 3))
    => '(1 2 3))

  (check
      (delete! 8 '(1 2 8 3 4 5 8 6 7 8))
    => '(1 2 3 4 5 6 7))


  (check
      (delete! 8 '() =)
    => '())

  (check
      (delete! 8 '(1) =)
    => '(1))

  (check
      (delete! 8 '(8) =)
    => '())

  (check
      (delete! 8 '(1 2 3) =)
    => '(1 2 3))

  (check
      (delete! 8 '(1 2 8 3 4 5 8 6 7 8) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete-duplicates '())
    => '())

  (check
      (delete-duplicates '(1))
    => '(1))

  (check
      (delete-duplicates '(1 2))
    => '(1 2))

  (check
      (delete-duplicates '(1 1))
    => '(1))

  (check
      (delete-duplicates '(1 1 1))
    => '(1))

  (check
      (delete-duplicates '(1 2 3 2 4 5 4 6 1 7))
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete-duplicates! '())
    => '())

  (check
      (delete-duplicates! '(1))
    => '(1))

  (check
      (delete-duplicates! '(1 2))
    => '(1 2))

  (check
      (delete-duplicates! '(1 1))
    => '(1))

  (check
      (delete-duplicates! '(1 1 1))
    => '(1))

  (check
      (delete-duplicates! '(1 2 3 2 4 5 4 6 1 7))
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete-duplicates '() =)
    => '())

  (check
      (delete-duplicates '(1) =)
    => '(1))

  (check
      (delete-duplicates '(1 2) =)
    => '(1 2))

  (check
      (delete-duplicates '(1 1) =)
    => '(1))

  (check
      (delete-duplicates '(1 1 1) =)
    => '(1))

  (check
      (delete-duplicates '(1 2 3 2 4 5 4 6 1 7) =)
    => '(1 2 3 4 5 6 7))

;;; --------------------------------------------------------------------

  (check
      (delete-duplicates! '() =)
    => '())

  (check
      (delete-duplicates! '(1) =)
    => '(1))

  (check
      (delete-duplicates! '(1 2) =)
    => '(1 2))

  (check
      (delete-duplicates! '(1 1) =)
    => '(1))

  (check
      (delete-duplicates! '(1 1 1) =)
    => '(1))

  (check
      (delete-duplicates! '(1 2 3 2 4 5 4 6 1 7) =)
    => '(1 2 3 4 5 6 7))

  )


#;(parameterise ((check-test-name 'alists))

  (check
      (assoc 'a
	     '((a . 1)
	       (b . 2)
	       (c . 3)))
    => '(a . 1))

  (check
      (assoc 'b
	     '((a . 1)
	       (b . 2)
	       (c . 3)))
    => '(b . 2))

  (check
      (assoc 'c
	     '((a . 1)
	       (b . 2)
	       (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (assoc* 'c
	      '())
    => #f)

  (check
      (assoc* 'd
	      '((a . 1)
		(b . 2)
		(c . 3)))
    => #f)

  (check
      (assoc* 'a
	      '((a . 1)
		(b . 2)
		(c . 3)))
    => '(a . 1))

  (check
      (assoc* 'b
	      '((a . 1)
		(b . 2)
		(c . 3)))
    => '(b . 2))

  (check
      (assoc* 'c
	      '((a . 1)
		(b . 2)
		(c . 3)))
    => '(c . 3))

  (check
      (assoc* 'a
	      '((a . 1)
		(b . 2)
		(c . 3))
	      eq?)
    => '(a . 1))

  (check
      (assoc* 'b
	      '((a . 1)
		(b . 2)
		(c . 3))
	      eq?)
    => '(b . 2))

  (check
      (assoc* 'c
	      '((a . 1)
		(b . 2)
		(c . 3))
	      eq?)
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (assq 'c
	    '())
    => #f)

  (check
      (assq 'd
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => #f)

  (check
      (assq 'a
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(a . 1))

  (check
      (assq 'b
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(b . 2))

  (check
      (assq 'c
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (assv 'c
	    '())
    => #f)

  (check
      (assv 'd
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => #f)

  (check
      (assv 'a
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(a . 1))

  (check
      (assv 'b
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(b . 2))

  (check
      (assv 'c
	    '((a . 1)
	      (b . 2)
	      (c . 3)))
    => '(c . 3))

;;; --------------------------------------------------------------------

  (check
      (alist-cons 'a 1
		  '((b . 2)
		    (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (alist-cons 'a 1
		  '())
    => '((a . 1)))

  (check
      (alist-cons 'b 2
		  '((b . 2)
		    (c . 3)))
    => '((b . 2)
	 (b . 2)
	 (c . 3)))

;;; --------------------------------------------------------------------

  (check
      (alist-copy '((a . 1)
		    (b . 2)
		    (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (alist-copy '((a . 1)))
    => '((a . 1)))

  (check
      (alist-copy '())
    => '())

;;; --------------------------------------------------------------------

  (check
      (alist-delete 'a
		    '((a . 1)
		      (b . 2)
		      (c . 3)))
    => '((b . 2)
	 (c . 3)))

  (check
      (alist-delete 'b
		    '((a . 1)
		      (b . 2)
		      (c . 3)))
    => '((a . 1)
	 (c . 3)))

  (check
      (alist-delete 'c
		    '((a . 1)
		      (b . 2)
		      (c . 3)))
    => '((a . 1)
	 (b . 2)))

  (check
      (alist-delete 'd
		    '((a . 1)
		      (b . 2)
		      (c . 3)))
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (alist-delete 'a
		    '((a . 1)
		      (a . 2)
		      (c . 3)))
    => '((c . 3)))

  (check
      (alist-delete 'a
		    '())
    => '())

  (check
      (alist-delete 'a
		    '((a . 1)))
    => '())

  (check
      (alist-delete 'a
		    '((a . 1)
		      (b . 2)
		      (c . 3))
		    eq?)
    => '((b . 2)
	 (c . 3)))

  (check
      (alist-delete 'b
		    '((a . 1)
		      (b . 2)
		      (c . 3))
		    eq?)
    => '((a . 1)
	 (c . 3)))

  (check
      (alist-delete 'c
		    '((a . 1)
		      (b . 2)
		      (c . 3))
		    eq?)
    => '((a . 1)
	 (b . 2)))

  (check
      (alist-delete 'd
		    '((a . 1)
		      (b . 2)
		      (c . 3))
		    eq?)
    => '((a . 1)
	 (b . 2)
	 (c . 3)))

  (check
      (alist-delete 'a
		    '((a . 1)
		      (a . 2)
		      (c . 3))
		    eq?)
    => '((c . 3)))

  (check
      (alist-delete 'a
		    '()
		    eq?)
    => '())

  (check
      (alist-delete 'a
		    '((a . 1))
		    eq?)
    => '())

  )


;;;; done

(check-report)

;;; end of file

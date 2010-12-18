;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for (classes)
;;;Date: Thu Apr  1, 2010
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
  (checks)
  (debugging)
  (records-lib)
  (rnrs eval))

(check-set-mode! 'report-failed)
(display "*** testing classes basics\n")

(debugging #t)

;;; --------------------------------------------------------------------

;; (import (rnrs)
;;   (parameters)
;;   (records-lib)
;;   (classes)
;;   (rnrs eval))

;; (define check-test-name
;;   (make-parameter #f))
;; (define debugging
;;   (make-parameter #f))
;; (define (debug-print-condition . args)
;;   #f)

;; (define check-count		0)
;; (define check-success-count	0)
;; (define check-failure-count	0)

;; (define-syntax check
;;   (syntax-rules (=>)
;;     ((_ ?expr => ?expected-result)
;;      (check ?expr (=> equal?) ?expected-result))
;;     ((_ ?expr (=> ?equal) ?expected-result)
;;      (let ((result	?expr)
;; 	   (expected	?expected-result))
;;        (set! check-count (+ 1 check-count))
;;        (if (?equal result expected)
;; 	   (set! check-success-count (+ 1 check-success-count))
;; 	 (begin
;; 	   (set! check-failure-count (+ 1 check-failure-count))
;; 	   (display "test error, expected\n\n")
;; 	   (write expected)
;; 	   (newline)
;; 	   (display "\ngot:\n\n")
;; 	   (write result)
;; 	   (newline)
;; 	   (display "\ntest body:\n\n")
;; 	   (write '(check ?expr (=> ?equal) ?expected-result))
;; 	   (newline)))))
;;     ))

;; (define-syntax check-for-true
;;   (syntax-rules ()
;;     ((_ ?form)
;;      (check (if ?form #t #f) => #t))
;;     ((_ (quote ?name) ?form)
;;      (check (quote ?name) (if ?form #t #f) => #t))))

;; (define-syntax check-for-false
;;   (syntax-rules ()
;;     ((_ ?form)
;;      (check (if ?form #t #f) => #f))
;;     ((_ (quote ?name) ?form)
;;      (check (quote ?name) (if ?form #t #f) => #f))))


;; (define (check-report)
;;   (display (string-append "*** executed " (number->string check-count)
;; 			  " tests, successful: " (number->string check-success-count)
;; 			  ", failed: "(number->string check-failure-count) "\n")))


(parametrise ((check-test-name	'definition-simple)
	      (debugging	#t))

  (let ()

    (define-class <alpha>
      (fields (mutable a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (<alpha>-a o))
      => 123)

    #f)

  (let ()

    (define-class (<alpha> make-<alpha> <alpha>?)
      (fields (mutable a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (<alpha>-a o))
      => 123)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check 	;invalid name
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else
		 (write E)(newline)
		 #f))
	(eval '(define-class 123
		 (fields a b c))
	      (environment '(nausicaa))))
    => 123)

  (check	;invalid constructor
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else
		 (write E)(newline)
		 #f))
	(eval '(define-class (<alpha> 123 <alpha>?)
		 (fields a b c))
	      (environment '(nausicaa))))
    => '(<alpha> 123 <alpha>?))

  (check 	;unknown clause
    (guard (E ((syntax-violation? E)
	       (syntax-violation-subform E))
	      (else
	       (debug-print-condition "unknown clause:" E)
	       #f))
      (eval '(define-class <alpha>
	       (woppa 123))
	    (environment '(nausicaa))))
    => '(woppa 123))

  #t)


(parametrise ((check-test-name	'definition-foreign-class))


;;; --------------------------------------------------------------------
;;; errors

  (check	;attempt to instantiate foreign class
      (guard (E ((syntax-violation? E)
;;;		   (write (condition-message E))(newline)
;;;		   (write E)(newline)
		 #t)
		(else
;;;		   (write E)(newline)
		 #f))
	(eval '(let ()
		 (define-foreign-class <alpha>
		   (predicate integer?))
		 (make <alpha>))
	      (environment '(nausicaa))))
    => #t)

  (check	;attempt to instantiate foreign class
      (guard (E ((syntax-violation? E)
;;;		   (write (condition-message E))(newline)
;;;		   (write E)(newline)
		 #t)
		(else
;;;		   (write E)(newline)
		 #f))
	(eval '(make <vector>) (environment '(nausicaa))))
    => #t)


  #t)


(parametrise ((check-test-name	'definition-inherit-clause)
	      (debugging	#t))

  (let ()	;inherit with INHERIT

    (define-class <alpha>
      (inherit <top>)
      (nongenerative alpha)
      (fields (mutable a)))

    (define-class <beta>
      (inherit <alpha>)
      (protocol (lambda (alpha-maker)
      		  (lambda (a b)
      		    (let ((beta-maker (alpha-maker a)))
    		      (beta-maker b)))))
      (sealed #t)
      (opaque #t)
      (nongenerative test:beta)
      (fields (immutable b)))

    (check
    	(let ((o (make-<beta> 1 2)))
    	  (list (<alpha>-a o)
    		(<beta>-b o)
    		))
      => '(1 2))

    #f)

;;; --------------------------------------------------------------------
;;; selective inheritance

  (let ()	;only concrete fields

    (define-class <alpha>
      (fields a b))

    (define-class <beta>
      (inherit <alpha>
	(nothing concrete-fields))
      (fields c d))

    (check
	(let ((p (make <beta> 1 2 3 4)))
	  (with-class ((p <beta>))
	    (list p.a p.b p.c p.d)))
      => '(1 2 3 4))

    (check	;attempt to reference uninherited field
	(guard (E ((undefined-violation? E)
		   #t)
		  (else #f))
	  (eval '(let ()
		   (define-class <alpha>
		     (fields a b)
		     (virtual-fields (immutable c <alpha>-b)))

		   (define-class <beta>
		     (inherit <alpha>
		       (nothing concrete-fields))
		     (fields d e))

		   (let ((p (make <beta> 1 2 3 4)))
		     (with-class ((p <beta>))
		       p.c)))
		(environment '(nausicaa))))
      => #t)

    #f)

  (let ()	;only virtual fields

    (define-class <alpha>
      (fields a b)
      (virtual-fields (immutable c <alpha>-b)))

    (define-class <beta>
      (inherit <alpha>
	(nothing virtual-fields))
      (fields d e))

    (check
	(let ((p (make <beta> 1 2 3 4)))
	  (with-class ((p <beta>))
	    (list p.c p.d p.e)))
      => '(2 3 4))

    (check
	(guard (E ((undefined-violation? E)
		   #t)
		  (else #f))
	  (eval '(let ()
		   (define-class <alpha>
		     (fields a b)
		     (virtual-fields (immutable c <alpha>-b)))

		   (define-class <beta>
		     (inherit <alpha>
		       (nothing virtual-fields))
		     (fields d e))

		   (let ((p (make <beta> 1 2 3 4)))
		     (with-class ((p <beta>))
		       p.a)))
		(environment '(nausicaa))))
      => #t)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid value
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (inherit 123)
		 (fields a b c))
	      (environment '(nausicaa))))
    => '(inherit 123))

  (check 	;multiple INHERIT is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple inherit is bad:" E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "multiple inherit is bad:" E)
		 #f))
	(eval '(let ()
                 (define-class <alpha>
		   (inherit ciao)
		   (inherit hello))
                 #f)
	      (environment '(nausicaa))))
    => '((inherit ciao)
	 (inherit hello)))

  #t)


(parametrise ((check-test-name	'definition-parent-clause)
	      (debugging	#t))

  (let ()	;inherit with PARENT

    (define-record-type <alpha>
      (fields (mutable a)))

    (define-class <beta>
      (parent <alpha>)
      (fields (immutable b)))

    (check
    	(let ((o (make-<beta> 1 2)))
    	  (list (<alpha>-a o)
    		(<beta>-b o)
    		))
      => '(1 2))

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid value
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (parent 123)
		 (fields a b c))
	      (environment '(nausicaa))))
    => '(parent 123))

  (check	;multiple PARENT is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple PARENT is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (parent ciao)
		 (parent hello))
	      (environment '(nausicaa))))
    => '((parent ciao)
	 (parent hello)))



  #t)


(parametrise ((check-test-name	'definition-parent-rtd-clause)
	      (debugging	#t))

  (let ()	;inherit with PARENT-RTD

    (define-record-type <alpha>
      (fields (mutable a)))

    (define-class <beta>
      (parent-rtd (record-type-descriptor <alpha>)
    		  (record-constructor-descriptor <alpha>))
      (fields (immutable b)))

    (check
    	(let ((o (make-<beta> 1 2)))
    	  (list (<alpha>-a o)
    		(<beta>-b o)
    		))
      => '(1 2))

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check 	;multiple PARENT-RTD is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple PARENT-RTD is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (parent-rtd ciao ciao)
		 (parent-rtd hello hello))
	      (environment '(nausicaa))))
    => '((parent-rtd ciao ciao)
	 (parent-rtd hello hello)))


  #t)


(parametrise ((check-test-name	'definition-protocol-clause))

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple PROTOCOL is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple PROTOCOL is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (protocol ciao)
		 (protocol hello))
	      (environment '(nausicaa))))
    => '((protocol ciao)
	 (protocol hello)))

  #t)


(parametrise ((check-test-name	'definition-public-protocol-clause))

  (let ()	;public protocol

    (define-class <alpha>
      (fields a b)
      (public-protocol (lambda (make-top)
			 (lambda (a b)
			   ((make-top) a b))))
      (superclass-protocol (lambda (make-top)
			     (lambda (a b)
			       #f))))

    (check
	(is-a? (make <alpha> 1 2) <alpha>)
      => #t)

    #f)

;;; --------------------------------------------------------------------

  (check	;PUBLIC-PROTOCOL in foreign class definition is bad
      (guard (E ((syntax-violation? E) #t)
		(else #f))
	(eval '(let ()
		 (define-foreign-class <alpha>
		   (public-protocol (lambda (n) (lambda () (n)))))
		 #f)
	      (environment '(nausicaa))))
    => #t)

  (check	;multiple PUBLIC-ROTOCOL is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple PUBLIC-PROTOCOL is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (public-protocol ciao)
		 (public-protocol hello))
	      (environment '(nausicaa))))
    => '((public-protocol ciao)
	 (public-protocol hello)))

  #t)


(parametrise ((check-test-name	'definition-superclass-protocol-clause))

  (let ()	;superclass protocol

    (define-class <alpha>
      (fields a b)
      (public-protocol (lambda (make-top)
			 (lambda (a b)
			   #f)))
      (superclass-protocol (lambda (make-top)
			     (lambda (a b)
			       ((make-top) a b)))))

    (define-class <beta>
      (inherit <alpha>)
      (fields c d)
      (public-protocol (lambda (make-alpha)
			 (lambda (a b c d)
			   ((make-alpha a b) c d))))
      (superclass-protocol (lambda (make-alpha)
			     (lambda (a b c d)
			       #f))))

    (check
	(is-a? (make <beta> 1 2 3 4) <beta>)
      => #t)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple SUPERCLASS-ROTOCOL is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple SUPERCLASS-PROTOCOL is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (superclass-protocol ciao)
		 (superclass-protocol hello))
	      (environment '(nausicaa))))
    => '((superclass-protocol ciao)
	 (superclass-protocol hello)))

  #t)


(parametrise ((check-test-name	'definition-from-fields-constructor))

  (let ()	;from-fields constructor

    (define-class <alpha>
      (fields a b)
      (protocol (lambda (make-top)
		  (lambda ()
		    ((make-top) 1 2)))))

    (define-class <beta>
      (inherit <alpha>)
      (fields c d)
      (protocol (lambda (make-alpha)
		  (lambda ()
		    ((make-alpha) 3 4)))))

    (check
	(let/with-class (((o <beta>) (make <beta>)))
	  (list o.a o.b o.c o.d))
      => '(1 2 3 4))

    (check
	(let/with-class (((o <beta>) (make-from-fields <beta>
				       #\a #\b #\c #\d)))
	  (list o.a o.b o.c o.d))
      => '(#\a #\b #\c #\d))

    #f)


  #t)


(parametrise ((check-test-name	'definition-sealed-clause))

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid sealed
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (sealed 123)
		 (fields a b c))
	      (environment '(nausicaa))))
    => '(sealed 123))

  (check	;multiple SEALED is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple SEALED is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (sealed #t)
		 (sealed #f))
	      (environment '(nausicaa))))
    => '((sealed #t) (sealed #f)))


  #t)


(parametrise ((check-test-name	'definition-opaque-clause))

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid opaque
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (opaque 123)
		 (fields a b c))
	      (environment '(nausicaa))))
    => '(opaque 123))

  (check	;multiple OPAQUE is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple OPAQUE is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (opaque #t)
		 (opaque #f))
	      (environment '(nausicaa))))
    => '((opaque #t)
	 (opaque #f)))

  #t)


(parametrise ((check-test-name	'definition-predicate-clause))

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid value
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (predicate 123)
		 (fields a b c))
	      (environment '(nausicaa))))
    => '(predicate 123))

  (check	;multiple PREDICATE is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple PREDICATE is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (predicate ciao)
		 (predicate hello))
	      (environment '(nausicaa))))
    => '((predicate ciao)
	 (predicate hello)))

  #t)


(parametrise ((check-test-name	'definition-nongenerative-clause))

;;; --------------------------------------------------------------------
;;; errors

  (check	;invalid value
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (nongenerative 123)
		 (fields a b c))
	      (environment '(nausicaa))))
    => '(nongenerative 123))

  (check	;multiple non-empty NONGENERATIVE is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple NONGENERATIVE is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (nongenerative ciao)
		 (nongenerative hello))
	      (environment '(nausicaa))))
    => '((nongenerative ciao)
	 (nongenerative hello)))

  (check	;multiple empty NONGENERATIVE is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple NONGENERATIVE is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (nongenerative)
		 (nongenerative))
	      (environment '(nausicaa))))
    => '((nongenerative)
	 (nongenerative)))

  #t)


(parametrise ((check-test-name	'definition-bindings-clause))

  (let ()

    (define-class <alpha>
      (fields a b c)
      (bindings <alpha>-bindings))

    (define-syntax <alpha>-bindings
      (lambda (stx)
	(syntax-case stx ()
	  ((_ ?class-name ?identifier . ?body)
	   (with-syntax ((A (datum->syntax #'?identifier 'a)))
	     #`(let ((A 123)) . ?body))))))

    (check
	(let ((o (make <alpha> 1 2 3)))
	  (with-class ((o <alpha>))
	    a))
      => 123)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple BINDINGS is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple BINDINGS is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
                 (bindings ciao)
		 (bindings hello))
	      (environment '(nausicaa))))
    => '((bindings ciao)
	 (bindings hello)))

  #t)


(parametrise ((check-test-name	'definitions-fields-clause))

  (let ()

    (define-class <alpha>
      (fields (mutable a)
	      (immutable b)
	      c))

    (check
	(let ((o (make-<alpha> 1 2 3)))
	  (list (<alpha>-a o)
		(<alpha>-b o)
		(<alpha>-c o)))
      => '(1 2 3))

    (check
	(let ((o (make-<alpha> 1 2 3)))
	  (<alpha>-a-set! o 10)
	  (list (<alpha>-a o)
		(<alpha>-b o)
		(<alpha>-c o)))
      => '(10 2 3))

    #f)

  (check
      (eval '(letrec ()
  	       (define-class <alpha>
  		 (fields (mutable a)
  			 (immutable b)
  			 c))
  	       (define o (make-<alpha> 1 2 3))
  	       (with-class ((o <alpha>))
  		 (set! o.a #t)
  		 o.a))
  	    (environment '(nausicaa)))
    => #t)

;;; --------------------------------------------------------------------
;;; accessor and mutator names

  (let ()

    (define-class <alpha>
      (fields (mutable a access-a mutate-a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (access-a o))
      => 123)

    (check
	(let ((o (make-<alpha> 123)))
	  (mutate-a o 456)
	  (access-a o))
      => 456)

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable a access-a)))

    (check
	(let ((o (make-<alpha> 123)))
	  (access-a o))
      => 123)

    #f)

;;; --------------------------------------------------------------------
;;; WITH-CLASS macro

  (let ()	;one field

    (define-class <alpha>
      (fields (mutable a)))

    (define r (make-<alpha> 123))
    (define s (make-<alpha> #\a))
    (define t (make-<alpha> 1.0))

    (with-class ((r <alpha>)
		 (s <alpha>)
		 (t <alpha>))
      (check
	  (list r.a s.a t.a)
	=> '(123 #\a 1.0))

      (set! r.a 456)
      (set! s.a #\b)
      (set! t.a 2.0)

      (check
	  (list r.a s.a t.a)
	=> '(456 #\b 2.0))

      #f))

  (let ()	;more fields
    (define-class <alpha>
      (fields (mutable a)
	      (mutable b)))

    (define r (make-<alpha> 1 2))
    (define s (make-<alpha> #\a #\b))
    (define t (make-<alpha> 1.0 2.0))
    (with-class ((r <alpha>)
		 (s <alpha>)
		 (t <alpha>))
      (check
	  (list r.a s.a t.a
		r.b s.b t.b)
	=> '(1 #\a 1.0  2 #\b 2.0))

      (set! r.a 3)
      (set! s.a #\c)
      (set! t.a 3.0)

      (check
	  (list r.a s.a t.a
		r.b s.b t.b)
	=> '(3 #\c 3.0  2 #\b 2.0))

      (set! r.b 4)
      (set! s.b #\d)
      (set! t.b 4.0)

      (check
	  (list r.a s.a t.a
		r.b s.b t.b)
	=> '(3 #\c 3.0  4 #\d 4.0))

      #f)
    #f)

;;; these tests use the record definitions from (records-lib)

  (check
      (let ((r (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(with-class ((r <gamma> <beta> <alpha>))
	  (list r.a r.b r.c
		r.d r.e r.f
		r.g r.h r.i)))
    => '(1 2 3 4 5 6 7 8 9))

  (check
      (let ((r (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(with-class ((r <gamma> <alpha>))
	  (list r.a r.b r.c
		r.g r.h r.i)))
    => '(1 2 3 7 8 9))

;;; common inheritance

  (let ()	;fields

    (define-class <alpha>
      (fields a b z))

    (define-class <beta>
      (inherit <alpha>)
      (fields c d z))

    (check	;accessing fields of both class and superclass
    	(let ((p (make <beta> 1 2 3 4 5 6)))
    	  (with-class ((p <beta>))
    	    (list p.a p.b p.c p.d)))
      => '(1 2 4 5))

    (check	;precedence of subclass fields
    	(let ((p (make <beta> 1 2 3 4 5 6)))
    	  (with-class ((p <beta>))
    	    p.z))
      => 6)

    (check	;custom precedence of superclass fields
    	(let ((p (make <beta> 1 2 3 4 5 6)))
    	  (with-class ((p <beta> <alpha>))
    	    p.z))
      => 3)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple FIELDS is fine
      (let ()
	(define-class <alpha>
	  (fields a b)
	  (fields c d))
	#t)
    => #t)

  (check	;attempt to mutate immutable field
      (guard (E ((syntax-violation? E) #t)
  		(else #f))
  	(eval '(letrec ()
  		 (define-class <alpha>
  		   (fields (mutable a)
  			   (immutable b)
  			   c))
  		 (define o (make-<alpha> 1 2 3))
  		 (with-class ((o <alpha>))
		   (set! o.b #f)))
  	      (environment '(nausicaa))))
    => #t)

  (check	;attempt to mutate immutable field
      (guard (E ((syntax-violation? E) #t)
  		(else #f))
  	(eval '(letrec ()
  		 (define-class <alpha>
  		   (fields (mutable a)
  			   (immutable b)
  			   c))
  		 (define o (make-<alpha> 1 2 3))
  		 (with-class ((o <alpha>))
		   (set! o.c #f)))
  	      (environment '(nausicaa) '(classes))))
    => #t)


  #t)


(parametrise ((check-test-name	'definitions-virtual-fields-clause))

  (let ()	;immutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;mutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (<fraction>-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named immutable virtual fields accessor

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator the-numerator)
		      (immutable denominator the-denominator)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named virtual fields accessor and mutator

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator the-numerator the-numerator-set!)
		      (mutable denominator the-denominator the-denominator-set!)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (define (the-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator-set! o 5)
	  (<fraction>-number o))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------

  (let ()	;explicitly named virtual fields accessor and mutator
		;mixed mutable and immutable

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator the-numerator)
		      (mutable denominator the-denominator the-denominator-set!)))

    (define (the-numerator o)
      (numerator (<fraction>-number o)))

    (define (the-denominator o)
      (denominator (<fraction>-number o)))

    (define (the-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-numerator o))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator o))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (the-denominator-set! o 5)
	  (<fraction>-number o))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------
;;; WITH-CLASS usage

  (let ()

    (define-class <alpha>
      (fields a b z)
      (virtual-fields (immutable x <alpha>-a)
		      (immutable y <alpha>-b)))

    (define-class <beta>
      (inherit <alpha>)
      (fields c d z)
      (virtual-fields (immutable m <beta>-c)
		      (immutable y <beta>-d)))

    (check	;accessing fields of both class and superclass
    	(let ((p (make <beta>
		   1 2 3
		   4 5 6)))
    	  (with-class ((p <beta>))
    	    (list p.x p.m)))
      => '(1 4))

    (check	;precedence of subclass fields
    	(let ((p (make <beta>
		   1 2 3
		   4 5 6)))
    	  (with-class ((p <beta>))
    	    p.y))
      => 5)

    (check	;custom precedence of superclass fields
    	(let ((p (make <beta>
		   1 2 3
		   4 5 6)))
    	  (with-class ((p <beta> <alpha>))
    	    p.y))
      => 2)

    #f)

  (let ()	;immutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.numerator))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.denominator))
      => 3)

    #f)

  (let ()	;mutable virtual fields

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define (<fraction>-denominator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ (numerator n) v))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.numerator))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.denominator))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (set! o.numerator 5)
	    o.number))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (set! o.denominator 5)
	    o.number))
      => 2/5)

    #f)

;;; the following tests use the records from (records-lib)

  (let ()
    (define r (make <alpha> 123 #\a 1.0))

    (with-class ((r <alpha>))

      (check
      	  (list r.a r.b r.c)
      	=> '(123 #\a 1.0))

      (set! r.a 456)
      (set! r.c 2.0)

      (check
	  (list r.a r.b r.c)
	=> '(456 #\a 2.0))

      #f)
    #f)

;;; --------------------------------------------------------------------
;;; virtual field name collision

  (let ()	;concrete fields name collision

    (define-record-type <alpha>
      (fields a))

    (define-record-type <beta>
      (parent <alpha>)
      (fields a))

    (let ((o (make-<beta> 1 2)))

      (check
	  (<alpha>-a o)
	=> 1)

      (check
	  (<beta>-a o)
	=> 2)

      #f)

    #f)

  (let ()	;virtual fields

    (define-class <alpha>
      (virtual-fields a))

    (define-class <beta>
      (inherit <alpha>)
      (virtual-fields a))

    (define (<alpha>-a o)
      1)

    (define (<beta>-a o)
      2)

    (let ((o (make-<beta>)))

      (check
	  (with-class ((o <alpha>))
	    o.a)
	=> 1)

      (check
	  (with-class ((o <beta>))
	    o.a)
	=> 2)

      (check
	  (with-class ((o <alpha>)
		       (o <beta>))
	    o.a)
	=> 2)

      (check
	  (with-class ((o <beta>)
		       (o <alpha>))
	    o.a)
	=> 1)

      (check
	  (with-class ((o <alpha> <beta>))
	    o.a)
	=> 2)

      (check
	  (with-class ((o <beta> <alpha>))
	    o.a)
	=> 1)

      #f)

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple VIRTUAL-FIELDS is fine
      (let ()
	(define-class <alpha>
	  (virtual-fields a b)
	  (virtual-fields c d))
	#t)
    => #t)

  (check	;attempt to mutate immutable virtual-field
      (guard (E ((syntax-violation? E) #t)
  		(else #f))
  	(eval '(letrec ()
  		 (define-class <alpha>
  		   (fields (mutable a)
  			   c)
		   (virtual-fields (immutable b)))
		 (define (<alpha>-b o)
		   #t)
  		 (define o (make-<alpha> 1 2 3))
  		 (with-class ((o <alpha>))
		   (set! o.b #f)))
  	      (environment '(nausicaa))))
    => #t)

  (check	;attempt to mutate immutable virtual field
      (guard (E ((syntax-violation? E) #t)
  		(else #f))
  	(eval '(letrec ()
  		 (define-class <alpha>
  		   (fields (mutable a)
  			   (immutable b))
		   (virtual-fields c))
		 (define (<alpha>-c o)
		   #t)
  		 (define o (make-<alpha> 1 2 3))
  		 (with-class ((o <alpha>))
		   (set! o.c #f)))
  	      (environment '(nausicaa) '(classes))))
    => #t)

  #t)


(parametrise ((check-test-name	'definitions-methods-clause))

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (methods numerator denominator))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (methods (numerator)
	       (denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (methods (numerator   fraction-numerator)
	       (denominator fraction-denominator)))

    (define (fraction-numerator o)
      (numerator (<fraction>-number o)))

    (define (fraction-denominator o)
      (denominator (<fraction>-number o)))

    #f)

;;; --------------------------------------------------------------------
;;; WITH-CLASS usage

  (let ()	;methods

    (define-class <alpha>
      (fields a b z)
      (methods (x <alpha>-a)
	       (y <alpha>-b)))

    (define-class <beta>
      (inherit <alpha>)
      (fields c d z)
      (methods (m <beta>-c)
	       (y <beta>-d)))

    (check	;accessing methods of both class and superclass
    	(let ((p (make <beta>
		   1 2 3
		   4 5 6)))
    	  (with-class ((p <beta>))
    	    (list (p.x) (p.m))))
      => '(1 4))

    (check	;precedence of subclass methods
    	(let ((p (make <beta>
		   1 2 3
		   4 5 6)))
    	  (with-class ((p <beta>))
    	    (p.y)))
      => 5)

    (check	;custom precedence of superclass methods
    	(let ((p (make <beta>
		   1 2 3
		   4 5 6)))
    	  (with-class ((p <beta> <alpha>))
    	    (p.y)))
      => 2)

    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator))
      (methods (denominator)
	       product
	       (the-list the-list-function)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (define/with-class (<fraction>-product (o <fraction>) lambda)
      (set! o.numerator (* o.numerator lambda)))

    (define/with-class (the-list-function (o <fraction>) . ell)
      (cons o.numerator ell))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.numerator))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.denominator)))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.product 10)
	    o.numerator))
      => 20)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.the-list 10 11 12 13)))
      => '(2 10 11 12 13))

    #f)

;;; --------------------------------------------------------------------
;;; method name collision

  (let ()

    (define-class <alpha>
      (methods a))

    (define-class <beta>
      (inherit <alpha>)
      (methods a))

    (define (<alpha>-a o)
      1)

    (define (<beta>-a o)
      2)

    (let ((o (make-<beta>)))

      (check
	  (with-class ((o <alpha>))
	    (o.a))
	=> 1)

      (check
	  (with-class ((o <beta>))
	    (o.a))
	=> 2)

      (check
	  (with-class ((o <alpha>)
		       (o <beta>))
	    (o.a))
	=> 2)

      (check
	  (with-class ((o <beta>)
		       (o <alpha>))
	    (o.a))
	=> 1)

      (check
	  (with-class ((o <alpha> <beta>))
	    (o.a))
	=> 2)

      (check
	  (with-class ((o <beta> <alpha>))
	    (o.a))
	=> 1)

      #f)
    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;multiple METHODS is fine
      (let ()
	(define-class <alpha>
	  (methods a b)
	  (methods c d))
	#t)
    => #t)

  #t)


(parametrise ((check-test-name	'definition-method-clause))

  (let ()	; methods

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator))
      (method (denominator o)
	(denominator (<fraction>-number o)))
      (method (product (o <fraction>) lambda)
	(set! o.numerator (* o.numerator lambda)))
      (method the-list
	(lambda/with-class ((o <fraction>) . ell)
	  (cons o.numerator ell))))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.numerator))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.denominator)))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.product 10)
	    o.numerator))
      => 20)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.the-list 10 11 12 13)))
      => '(2 10 11 12 13))

    #f)

  (let ()	; methods syntaxes

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator))
      (method-syntax denominator
	(syntax-rules ()
	  ((_ ?obj)
	   (let ((o ?obj))
	     (denominator (<fraction>-number o))))))
      (method (product (o <fraction>) lambda)
	(set! o.numerator (* o.numerator lambda)))
      (method the-list
	(lambda/with-class ((o <fraction>) . ell)
	  (cons o.numerator ell))))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-numerator-set! o v)
      (let ((n (<fraction>-number o)))
	(<fraction>-number-set! o (/ v (denominator n)))))

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.numerator))
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.denominator)))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.product 10)
	    o.numerator))
      => 20)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (o.the-list 10 11 12 13)))
      => '(2 10 11 12 13))

    #f)

;;; --------------------------------------------------------------------
;;; method name collision

  (let ()

    (define-class <alpha>
      (method (a o)
	1))

    (define-class <beta>
      (inherit <alpha>)
      (method (a o)
	2))

    (check
	(let ((o (make-<beta>)))
	  (with-class ((o <alpha>))
	    (o.a)))
      => 1)

    (check
	(let ((o (make-<beta>)))
	  (with-class ((o <beta>))
	    (o.a)))
      => 2)

    (check
	(let ((o (make-<beta>)))
	  (with-class ((o <alpha>)
		       (o <beta>))
	    (o.a)))
      => 2)

    (check
	(let ((o (make-<beta>)))
	  (with-class ((o <beta>)
		       (o <alpha>))
	    (o.a)))
      => 1)

    (check
	(let ((o (make-<beta>)))
	  (with-class ((o <alpha> <beta>))
	    (o.a)))
      => 2)

    (check
	(let ((o (make-<beta>)))
	  (with-class ((o <beta> <alpha>))
	    (o.a)))
      => 1)

    #f)
  #t)


(parametrise ((check-test-name	'definition-duplicated-id-errors)
	      (debugging	#t))

  (check	;duplicated field name, FIELDS/VIRTUAL-FIELDS
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "should have been duplicated name:" E)
		 #f))
	(eval '(define-class <alpha>
		 (fields a)
		 (virtual-fields a))
	      (environment '(nausicaa))))
    => 'a)

  (check	;duplicated field name, FIELDS/METHODS
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "should have been duplicated name:" E)
		 #f))
	(eval '(define-class <alpha>
		 (fields a)
		 (methods a))
	      (environment '(nausicaa))))
    => 'a)

  (check	;duplicated field name, FIELDS/METHOD
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "should have been duplicated name:" E)
		 #f))
	(eval '(define-class <alpha>
		 (fields a)
		 (method (a o)
		   #t))
	      (environment '(nausicaa))))
    => 'a)

  (check	;duplicated field name, VIRTUAL-FIELDS/METHODS
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "should have been duplicated name:" E)
		 #f))
	(eval '(define-class <alpha>
		 (virtual-fields a)
		 (methods a))
	      (environment '(nausicaa))))
    => 'a)

  (check	;duplicated field name, VIRTUAL-FIELDS/METHOD
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "should have been duplicated name:" E)
		 #f))
	(eval '(define-class <alpha>
		 (virtual-fields a)
		 (method (a o)
		   #t))
	      (environment '(nausicaa))))
    => 'a)

  (check	;duplicated field name, METHODS/METHOD
      (guard (E ((syntax-violation? E)
		 (syntax-violation-subform E))
		(else
		 (debug-print-condition "should have been duplicated name:" E)
		 #f))
	(eval '(define-class <alpha>
		 (methods a)
		 (method (a o)
		   #t))
	      (environment '(nausicaa))))
    => 'a)

  #t)


(parametrise ((check-test-name	'definition-maker)
	      (debugging	#t))

  (let ()	;only positional arguments

    (define-class <alpha>
      (fields a b)
      (maker (a b)))

    (check
	(let ((o (make <alpha> 1 2)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 2))

    #f)

  (let ()	;only optional arguments

    (define-class <alpha>
      (fields a b)
      (maker () (a 1) (b 2)))

    (check
	(let ((o (make <alpha>)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 2))

    (check
	(let ((o (make <alpha> (a 10))))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(10 2))

    (check
	(let ((o (make <alpha> (b 20))))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 20))

    (check
	(let ((o (make <alpha> (b 20) (a 10))))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(10 20))

    #f)

  (let ()	;mixed arguments

    (define-class <alpha>
      (fields a b)
      (maker (a) (b 2)))

    (check
	(let ((o (make <alpha> 1)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 2))

    (check
	(let ((o (make <alpha> 1 (b 20))))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 20))

    #f)

  (let ()	;maker protocol

    (define-class <alpha>
      (fields a b c)
      (maker (a) (b 2))
      (maker-protocol (lambda (make-top)
			(lambda (a b)
			  ((make-top) a b 456)))))

    (check
	(let ((o (make <alpha> 1)))
	  (with-class ((o <alpha>))
	    (list o.a o.b o.c)))
      => '(1 2 456))

    (check
	(let ((o (make <alpha> 1 (b 20))))
	  (with-class ((o <alpha>))
	    (list o.a o.b o.c)))
      => '(1 20 456))

    (check
	(let ((o (make* <alpha> 1 2 3)))
	  (with-class ((o <alpha>))
	    (list o.a o.b o.c)))
      => '(1 2 3))

    #f)

;;; --------------------------------------------------------------------
;;; no maker, defaults to the public protocol

  (let ()
    (define-class <alpha>
      (fields a b))

    (check	;when no MAKER is defined default to public constructor
	(is-a? (make <alpha> 1 2) <alpha>)
      => #t)

    #f)

;;; --------------------------------------------------------------------
;;; maker transformer

  (let ()	;identifier transformer

    (define-class <alpha>
      (fields a b)
      (maker (a)
	     (b 2))
      (maker-transformer transformer))

    (define-syntax transformer
      (syntax-rules ()
	((_ ?constructor ?a ?b)
	 (?constructor (+ 1 ?a) (+ 2 ?b)))))

    (check
	(let ((o (make* <alpha> 1 2)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 2))

    (check
	(let ((o (make <alpha> 1)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(2 4))

    (check
	(let ((o (make <alpha> 1 (b 20))))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(2 22))

    #f)

  (let ()	;expression transformer

    (define-class <alpha>
      (fields a b)
      (maker (a)
	     (b 2))
      (maker-transformer
       (lambda (stx)
	 (syntax-case stx ()
	   ((_ ?constructor ?a ?b)
	    #'(?constructor (+ 1 ?a) (+ 2 ?b)))))))

    (check
	(let ((o (make* <alpha> 1 2)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 2))

    (check
	(let ((o (make <alpha> 1)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(2 4))

    (check
	(let ((o (make <alpha> 1 (b 20))))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(2 22))

    #f)

  (let ()	;maker-transformer without maker

    (define-class <alpha>
      (fields a b)
      (maker-transformer transformer))

    (define-syntax transformer
      (syntax-rules ()
	((_ ?constructor ?a ?b)
	 (?constructor (+ 1 ?a) (+ 2 ?b)))))

    (check
	(let ((o (make* <alpha> 1 2)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 2))

    (check
	(let ((o (make <alpha> 1 2)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 2))

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;bad MAKER clause
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "bad MAKER clause:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (fields a b)
		 (maker 123 123))
	      (environment '(nausicaa))))
    => '(maker 123 123))

  (check	;multiple MAKER is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple MAKER is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (fields a b)
		 (maker (a) (b 1))
		 (maker (b) (a 2)))
	      (environment '(nausicaa))))
    => '((maker (a) (b 1))
	 (maker (b) (a 2))))

  #t)


(parametrise ((check-test-name	'custom-maker)
	      (debugging	#t))

  (let ()	;only positional arguments

    (define-maker (make-alpha a b)
      (make* <alpha>)
      ())

    (define-class <alpha>
      (fields a b)
      (custom-maker make-alpha))

    (check
	(let ((o (make <alpha> 1 2)))
	  (with-class ((o <alpha>))
	    (list o.a o.b)))
      => '(1 2))

    #f)

  (let ()	;only optional arguments

    (define-auxiliary-syntaxes
      a: b:)

    (define-maker make-alpha
      (make* <alpha>)
      ((a: 1)
       (b: 2)))

    (define-class <alpha>
      (fields a b)
      (custom-maker make-alpha))

    (check
  	(let ((o (make <alpha>)))
  	  (with-class ((o <alpha>))
  	    (list o.a o.b)))
      => '(1 2))

    (check
  	(let ((o (make <alpha> (a: 10))))
  	  (with-class ((o <alpha>))
  	    (list o.a o.b)))
      => '(10 2))

    (check
  	(let ((o (make <alpha> (b: 20))))
  	  (with-class ((o <alpha>))
  	    (list o.a o.b)))
      => '(1 20))

    (check
  	(let ((o (make <alpha> (b: 20) (a: 10))))
  	  (with-class ((o <alpha>))
  	    (list o.a o.b)))
      => '(10 20))

    #f)

  (let ()	;mixed arguments

    (define-auxiliary-syntaxes
      b)

    (define-maker (make-alpha a)
      (make* <alpha>)
      ((b 2)))

    (define-class <alpha>
      (fields a b)
      (custom-maker make-alpha))

    (check
  	(let ((o (make <alpha> 1)))
  	  (with-class ((o <alpha>))
  	    (list o.a o.b)))
      => '(1 2))

    (check
  	(let ((o (make <alpha> 1 (b 20))))
  	  (with-class ((o <alpha>))
  	    (list o.a o.b)))
      => '(1 20))

    #f)

;;; --------------------------------------------------------------------
;;; errors

  (check	;bad CUSTOM-MAKER clause
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "bad MAKER clause:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (fields a b)
		 (custom-maker 123))
	      (environment '(nausicaa))))
    => '(custom-maker 123))

  (check	;multiple MAKER is bad
      (guard (E ((syntax-violation? E)
;;;(debug-print-condition "multiple MAKER is bad:" E)
		 (syntax-violation-subform E))
		(else #f))
	(eval '(define-class <alpha>
		 (fields a b)
		 (custom-maker a)
		 (custom-maker b))
	      (environment '(nausicaa))))
    => '((custom-maker a)
	 (custom-maker b)))

  #t)


(parametrise ((check-test-name	'with-class))

  (let ()

    (define-class <alpha>
      (fields a))

    (check
	(let ((o (make <alpha> 1)))
	  (with-class ((o <alpha>))
	    o.a))
      => 1)

    (check
	(let ((o (make <alpha> 1)))
	  (with-class ((o <alpha>))
	    (define b 2)
	    (list b o.a)))
      => '(2 1))

    #f)

  #t)


(parametrise ((check-test-name	'let/with-class))

;;; let/with-class

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let/with-class (((a <fraction>) (make-<fraction> 2/3))
			 ((b <fraction>) (make-<fraction> 4/5)))
	  (list a.numerator b.denominator))
      => '(2 5))

    #f)

  (check	;use the records from (records-lib)
      (let/with-class (((r <gamma> <beta> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(list r.a r.b r.c
	      r.d r.e r.f
	      r.g r.h r.i))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (records-lib)
      (let/with-class (((r <gamma> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9))
		       ((s <beta>  <alpha>) (make <beta>  10 20 30 40 50 60)))
	(list r.a r.g s.a s.d))
    => '(1 7 10 40))


  (check
      (let/with-class ((a 1) (b 2))
	(list a b))
    => '(1 2))

  (check
      (let/with-class ((a 1)
		       ((r <gamma> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(list a r.b))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; named let/with-class

  (check
      (let/with-class loop ((a 0) (b 3))
	(if (zero? b)
	    a
	  (loop (+ 1 a) (- b 1))))
    => 3)

  (check
      (let/with-class loop ((a 0) ((b <integer>) 3))
		      (if b.zero?
			  a
			(loop (+ 1 a) (- b 1))))
    => 3)

  (check
      (let/with-class loop (((a <number>) 0) ((b <integer>) 3))
		      (if b.zero?
			  a.odd?
			(loop (+ 1 a) (- b 1))))
    => #t)

;;; --------------------------------------------------------------------
;;; let/with-class, type violation

;;   (let ()

;;     (define-class <alpha>
;;       (fields a))

;;     (define-class <beta>
;;       (inherit <alpha>)
;;       (fields b))

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; ;;;		   (write (condition-message E))(newline)
;; 		   #t)
;; 		  (else E))
;; 	  (let/with-class (((o <alpha>) 1))
;; 	    o.a))
;;       => #t)

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; ;;;		   (write (condition-message E))(newline)
;; 		   #t)
;; 		  (else E))
;; 	  (let/with-class (((o <alpha> <beta>) 1))
;; 	    o.a))
;;       => #t)

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; 		   #t)
;; 		  (else E))
;; 	  (let/with-class (((o <alpha>) (make <alpha> 1)))
;; 	    (set! o 123)
;; 	    o.a))
;;       => #t)

;;     #f)

;;; --------------------------------------------------------------------
;;; let*/with-class

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(let*/with-class (((a <fraction>) (make-<fraction> 2/3))
			  ((b <fraction>) (make-<fraction> 4/5)))
	  (list a.numerator b.denominator))
      => '(2 5))

    (check
	(let*/with-class (((a <fraction>) (make-<fraction> 2/3))
			  ((b <fraction>) (make-<fraction> (/ a.numerator 5))))
	  b.number)
      => 2/5)

    #f)

  (check	;use the records from (records-lib)
      (let*/with-class (((r <gamma> <beta> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(list r.a r.b r.c
	      r.d r.e r.f
	      r.g r.h r.i))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (records-lib)
      (let*/with-class (((r <gamma> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9))
			((s <beta>  <alpha>) (make <beta>  10 20 30 40 50 60)))
	(list r.a r.g s.a s.d))
    => '(1 7 10 40))

  (check
      (let*/with-class ((a 1) (b 2))
	(list a b))
    => '(1 2))

  (check
      (let*/with-class ((a 1)
			((r <gamma> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(list a r.b))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; let*/with-class, type violation

;;   (let ()

;;     (define-class <alpha>
;;       (fields a))

;;     (define-class <beta>
;;       (inherit <alpha>)
;;       (fields b))

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; ;;;		   (write (condition-message E))(newline)
;; 		   #t)
;; 		  (else E))
;; 	  (let*/with-class (((o <alpha>) 1))
;; 	    o.a))
;;       => #t)

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; ;;;		   (write (condition-message E))(newline)
;; 		   #t)
;; 		  (else E))
;; 	  (let*/with-class (((o <alpha> <beta>) 1))
;; 	    o.a))
;;       => #t)

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; 		   #t)
;; 		  (else E))
;; 	  (let*/with-class (((o <alpha>) (make <alpha> 1)))
;; 	    (set! o 123)
;; 	    o.a))
;;       => #t)

;;     #f)

;;; --------------------------------------------------------------------
;;; letrec/with-class

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(letrec/with-class (((a <fraction>) (make-<fraction> 2/3))
			    ((b <fraction>) (make-<fraction> 4/5)))
	  (list a.numerator b.denominator))
      => '(2 5))

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)))

    (define-class <beta>
      (fields (immutable proc)))

    (check
	(letrec/with-class (((a <alpha>) (make-<alpha> 123))
			    ((b <beta>)  (make-<beta> (lambda () a.value))))
	  (b.proc))
      => 123)

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)
	      (immutable proc)))

    (define-class <beta>
      (fields (immutable value)
	      (immutable proc)))

    (check
	(letrec/with-class (((a <alpha>) (make-<alpha>
					  1 (lambda ()
					      (cons a.value b.value))))
			    ((b <beta>)  (make-<beta>
					  2 (lambda ()
					      (cons a.value b.value)))))
	  (list (a.proc) (b.proc)))
      => '((1 . 2) (1 . 2)))

    #f)

  (check	;use the records from (records-lib)
      (letrec/with-class (((r <gamma> <beta> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(list r.a r.b r.c
	      r.d r.e r.f
	      r.g r.h r.i))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (records-lib)
      (letrec/with-class (((r <gamma> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9))
			  ((s <beta>  <alpha>) (make <beta>  10 20 30 40 50 60)))
	(list r.a r.g s.a s.d))
    => '(1 7 10 40))

  (check
      (letrec/with-class ((a 1) (b 2))
	(list a b))
    => '(1 2))

  (check
      (letrec/with-class ((a 1)
			  ((r <gamma> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(list a r.b))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; letrec/with-class, type violation

;;   (let ()

;;     (define-class <alpha>
;;       (fields a))

;;     (define-class <beta>
;;       (inherit <alpha>)
;;       (fields b))

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; ;;;		   (write (condition-message E))(newline)
;; 		   #t)
;; 		  (else E))
;; 	  (letrec/with-class (((o <alpha>) 1))
;; 	    o.a))
;;       => #t)

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; ;;;		   (write (condition-message E))(newline)
;; 		   #t)
;; 		  (else E))
;; 	  (letrec/with-class (((o <alpha> <beta>) 1))
;; 	    o.a))
;;       => #t)

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; 		   #t)
;; 		  (else E))
;; 	  (letrec/with-class (((o <alpha>) (make <alpha> 1)))
;; 	    (set! o 123)
;; 	    o.a))
;;       => #t)

;;     #f)

;;; --------------------------------------------------------------------
;;; letrec*/with-class

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define (<fraction>-numerator o)
      (numerator (<fraction>-number o)))

    (define (<fraction>-denominator o)
      (denominator (<fraction>-number o)))

    (check
	(letrec*/with-class (((a <fraction>) (make-<fraction> 2/3))
			     ((b <fraction>) (make-<fraction> 4/5)))
	  (list a.numerator b.denominator))
      => '(2 5))

    (check
	(letrec*/with-class (((a <fraction>) (make-<fraction> 2/3))
			     ((b <fraction>) (make-<fraction> (/ a.numerator 5))))
	  b.number)
      => 2/5)

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)
	      (immutable proc)))

    (define-class <beta>
      (fields (immutable value)
	      (immutable proc)))

    (check
	(letrec*/with-class (((a <alpha>) (make-<alpha>
					   1 (lambda () b.value)))
			     ((b <beta>)  (make-<beta>
					   2 (lambda () a.value))))
	  (list (a.proc) (b.proc)))
      => '(2 1))

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)
	      (immutable proc)))

    (define-class <beta>
      (fields (immutable value)
	      (immutable proc)))

    (check
	(letrec*/with-class (((a <alpha>) (make-<alpha>
					   1 (lambda ()
					       (cons a.value b.value))))
			     ((b <beta>)  (make-<beta>
					   2 (lambda ()
					       (cons a.value b.value)))))
	  (list (a.proc) (b.proc)))
      => '((1 . 2) (1 . 2)))

    #f)

  (let ()

    (define-class <alpha>
      (fields (immutable value)
	      (immutable proc)))

    (define-class <beta>
      (fields (immutable value)
	      (immutable proc)))

    (check
	(letrec*/with-class (((a <alpha>) (make-<alpha>
					   1 (lambda ()
					       (cons a.value b.value))))
			     ((b <beta>)  (make-<beta>
					   2 (lambda ()
					       (cons a.value b.value))))
			     ((c <top>)   (list (a.proc) (b.proc))))
	  c)
      => '((1 . 2) (1 . 2)))

    #f)

  (check	;use the records from (records-lib)
      (letrec*/with-class (((r <gamma> <beta> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(list r.a r.b r.c
	      r.d r.e r.f
	      r.g r.h r.i))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (records-lib)
      (letrec*/with-class (((r <gamma> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9))
			   ((s <beta>  <alpha>) (make <beta>  10 20 30 40 50 60)))
	(list r.a r.g s.a s.d))
    => '(1 7 10 40))

  (check
      (letrec*/with-class ((a 1) (b 2))
	(list a b))
    => '(1 2))

  (check
      (letrec*/with-class ((a 1)
			   ((r <gamma> <alpha>) (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(list a r.b))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; letrec*/with-class, type violation

;;   (let ()

;;     (define-class <alpha>
;;       (fields a))

;;     (define-class <beta>
;;       (inherit <alpha>)
;;       (fields b))

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; ;;;		   (write (condition-message E))(newline)
;; 		   #t)
;; 		  (else E))
;; 	  (letrec*/with-class (((o <alpha>) 1))
;; 	    o.a))
;;       => #t)

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; ;;;		   (write (condition-message E))(newline)
;; 		   #t)
;; 		  (else E))
;; 	  (letrec*/with-class (((o <alpha> <beta>) 1))
;; 	    o.a))
;;       => #t)

;;     (check
;; 	(guard (E ((assertion-violation? E)
;; 		   #t)
;; 		  (else E))
;; 	  (letrec*/with-class (((o <alpha>) (make <alpha> 1)))
;; 	    (set! o 123)
;; 	    o.a))
;;       => #t)

;;     #f)

;;; --------------------------------------------------------------------
;;; do/with-class

  (check
      (do/with-class ((a 0 (+ 1 a))
		      (b 3 (- b 1)))
		     ((zero? b) a))
    => 3)

  (check
      (do/with-class ((a 0 (+ 1 a))
		      ((b <integer>) 3 (- b 1)))
		     (b.zero? a))
    => 3)

  (check
      (do/with-class (((a <number>) 0 (+ 1 a))
		      ((b <integer>) 3 (- b 1)))
		     (b.zero? a.odd?))
    => #t)

;;; --------------------------------------------------------------------
;;; do*/with-class

  (check
      (do*/with-class ((a 0 (+ 1 a))
		       (b 3 (- b 1)))
		      ((zero? b) a))
    => 3)

  (check
      (do*/with-class ((a 0 (+ 1 a))
		       ((b <integer>) 3 (- b 1)))
		      (b.zero? a))
    => 3)

  (check
      (do*/with-class (((a <number>) 0 (+ 1 a))
		       ((b <integer>) 3 (- b 1)))
		      (b.zero? a.odd?))
    => #t)

  #t)


(parametrise ((check-test-name	'lambda-with))

;;; untyped

  (let ((f (lambda/with-class (a)
	     a)))
    (check (f 123) => 123)
    #f)

  (let ((f (lambda/with-class (a b)
	     (list a b))))
    (check (f 1 2) => '(1 2))
    #f)

  (let ((f (lambda/with-class (a b c)
	     (list a b c))))
    (check (f 1 2 3) => '(1 2 3))
    #f)

  (let ((f (lambda/with-class args
	     (list->vector args))))
    (check (f) => '#())
    (check (f 1) => '#(1))
    (check (f 1 2) => '#(1 2))
    (check (f 1 2 3) => '#(1 2 3))
    #f)

  (let ((f (lambda/with-class (a . rest)
	     (vector a rest))))
    (check (f 1) => '#(1 ()))
    (check (f 1 2) => '#(1 (2)))
    (check (f 1 2 3 4) => '#(1 (2 3 4)))
    #f)

  (let ((f (lambda/with-class (a b . rest)
	     (vector a b rest))))
    (check (f 1 2) => '#(1 2 ()))
    (check (f 1 2 3) => '#(1 2 (3)))
    (check (f 1 2 3 4) => '#(1 2 (3 4)))
    #f)

;;; --------------------------------------------------------------------
;;; typed

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define <fraction>-numerator
      (lambda/with-class ((o <fraction>))
	(numerator o.number)))

    (define <fraction>-numerator-set!
      (lambda/with-class ((o <fraction>) v)
	(set! o.number (/ v (denominator o.number)))))

    (define <fraction>-denominator
      (lambda/with-class ((o <fraction>))
	(denominator o.number)))

    (define <fraction>-denominator-set!
      (lambda/with-class ((o <fraction>) (v <top>))
	(set! o.number (/ (numerator o.number) v))))

    (let ((f (lambda/with-class ((a <fraction>))
	       a.numerator)))
      (check (f (make-<fraction> 2/3)) => 2)
      #f)

    (let ((f (lambda/with-class ((a <fraction>) (b <complex>))
	       (list a.numerator b.magnitude))))
      (check (f (make-<fraction> 2/3) -4) => '(2 4))
      #f)

    (let ((f (lambda/with-class ((a <fraction>) b (c <fraction>))
	       (list a.numerator b c.denominator))))
      (check (f (make-<fraction> 2/3) 4 (make-<fraction> 5/6)) => '(2 4 6))
      #f)

    (let ((f (lambda/with-class ((a <fraction>) . rest)
	       (vector a.numerator rest))))
      (check (f (make-<fraction> 11/12)) => '#(11 ()))
      (check (f (make-<fraction> 11/12) 2) => '#(11 (2)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 (2 3 4)))
      #f)

    (let ((f (lambda/with-class ((a <fraction>) b . rest)
	       (vector a.numerator b rest))))
      (check (f (make-<fraction> 11/12) 2) => '#(11 2 ()))
      (check (f (make-<fraction> 11/12) 2 3) => '#(11 2 (3)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 2 (3 4)))
      #f)

    ;;With definition in the body.
    (let ((f (lambda/with-class ((a <fraction>) b . rest)
	       (define r rest)
	       (vector a.numerator b r))))
      (check (f (make-<fraction> 11/12) 2) => '#(11 2 ()))
      (check (f (make-<fraction> 11/12) 2 3) => '#(11 2 (3)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 2 (3 4)))
      #f)

;;; --------------------------------------------------------------------

    (check
	(let/with-class (((o <fraction>) (make-<fraction> 2/3)))
	  o.numerator)
      => 2)

    (check
	(let/with-class (((o <fraction>) (make-<fraction> 2/3)))
	  o.numerator)
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.denominator))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (set! o.numerator 5)
	    o.number))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (set! o.denominator 5)
	    o.number))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------
;;; use the records from (records-lib)

  (check
      (let ((r (make <gamma> 1 2 3 4 5 6 7 8 9))
	    (f (lambda/with-class ((r <gamma> <beta> <alpha>))
		 (list r.a r.b r.c
		       r.d r.e r.f
		       r.g r.h r.i))))
	(f r))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (records-lib)
      (let ((r (make <gamma> 1 2 3 4 5 6 7 8 9))
	    (s (make <beta>  10 20 30 40 50 60))
	    (f (lambda/with-class ((r <gamma> <alpha>) (s <beta> <alpha>))
		 (list r.a r.g s.a s.d))))
	(f r s))
    => '(1 7 10 40))

  #t)


(parametrise ((check-test-name	'define-with))

  (let ()
    (define/with-class (f a)
      a)
    (check (f 123) => 123)
    #f)

  (let ()
    (define/with-class (f a b)
      (list a b))
    (check (f 1 2) => '(1 2))
    #f)

  (let ()
    (define/with-class (f a b c)
      (list a b c))
    (check (f 1 2 3) => '(1 2 3))
    #f)

  (let ()
    (define/with-class (f . args)
      (list->vector args))
    (check (f) => '#())
    (check (f 1) => '#(1))
    (check (f 1 2) => '#(1 2))
    (check (f 1 2 3) => '#(1 2 3))
    #f)

  (let ()
    (define/with-class (f a . rest)
      (vector a rest))
    (check (f 1) => '#(1 ()))
    (check (f 1 2) => '#(1 (2)))
    (check (f 1 2 3 4) => '#(1 (2 3 4)))
    #f)

  (let ()
    (define/with-class (f a b . rest)
      (vector a b rest))
    (check (f 1 2) => '#(1 2 ()))
    (check (f 1 2 3) => '#(1 2 (3)))
    (check (f 1 2 3 4) => '#(1 2 (3 4)))
    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define/with-class (<fraction>-numerator (o <fraction>))
      (numerator o.number))

    (define/with-class (<fraction>-numerator-set! (o <fraction>) (v <top>))
      (set! o.number (/ v (denominator o.number))))

    (define/with-class (<fraction>-denominator (o <fraction>))
      (denominator o.number))

    (define/with-class (<fraction>-denominator-set! (o <fraction>) (v <top>))
      (set! o.number (/ (numerator o.number) v)))

    (check
	(let/with-class (((o <fraction>) (make-<fraction> 2/3)))
	  o.numerator)
      => 2)

    (check
	(let/with-class (((o <fraction>) (make-<fraction> 2/3)))
	  o.numerator)
      => 2)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    o.denominator))
      => 3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (set! o.numerator 5)
	    o.number))
      => 5/3)

    (check
	(let ((o (make-<fraction> 2/3)))
	  (with-class ((o <fraction>))
	    (set! o.denominator 5)
	    o.number))
      => 2/5)

    #f)

;;; --------------------------------------------------------------------
;;; use the records from (records-lib)

  (check
      (let ((r (make <gamma> 1 2 3 4 5 6 7 8 9)))
	(define/with-class (f (r <gamma> <beta> <alpha>))
	  (list r.a r.b r.c
		r.d r.e r.f
		r.g r.h r.i))
	(f r))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (records-lib)
      (let ((r (make <gamma> 1 2 3 4 5 6 7 8 9))
	    (s (make <beta>  10 20 30 40 50 60)))
	(define/with-class (f (r <gamma> <alpha>) (s <beta> <alpha>))
	  (list r.a r.g s.a s.d))
	(f r s))
    => '(1 7 10 40))

  #t)


(parametrise ((check-test-name	'case-lambda-with))

;;; untyped

  (let ((f (case-lambda/with-class
	    ((a)
	     a))))
    (check (f 123) => 123)
    #f)

  (let ((f (case-lambda/with-class
	    ((a b)
	     (list a b)))))
    (check (f 1 2) => '(1 2))
    #f)

  (let ((f (case-lambda/with-class
	    ((a b c)
	     (list a b c)))))
    (check (f 1 2 3) => '(1 2 3))
    #f)

  (let ((f (case-lambda/with-class
	    (args
	     (list->vector args)))))
    (check (f) => '#())
    (check (f 1) => '#(1))
    (check (f 1 2) => '#(1 2))
    (check (f 1 2 3) => '#(1 2 3))
    #f)

  (let ((f (case-lambda/with-class
	    ((a . rest)
	     (vector a rest)))))
    (check (f 1) => '#(1 ()))
    (check (f 1 2) => '#(1 (2)))
    (check (f 1 2 3 4) => '#(1 (2 3 4)))
    #f)

  (let ((f (case-lambda/with-class
	    ((a b . rest)
	     (vector a b rest)))))
    (check (f 1 2) => '#(1 2 ()))
    (check (f 1 2 3) => '#(1 2 (3)))
    (check (f 1 2 3 4) => '#(1 2 (3 4)))
    #f)

;;; --------------------------------------------------------------------
;;; typed

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (mutable numerator)
		      (mutable denominator)))

    (define <fraction>-numerator
      (case-lambda/with-class
       (((o <fraction>))
	(numerator o.number))))

    (define <fraction>-numerator-set!
      (case-lambda/with-class
       (((o <fraction>) v)
	(set! o.number (/ v (denominator o.number))))))

    (define <fraction>-denominator
      (case-lambda/with-class
       (((o <fraction>))
	(denominator o.number))))

    (define <fraction>-denominator-set!
      (case-lambda/with-class
       (((o <fraction>) (v <top>))
	(set! o.number (/ (numerator o.number) v)))))

    (let ((f (case-lambda/with-class
	      (((a <fraction>))
	       a.numerator))))
      (check (f (make-<fraction> 2/3)) => 2)
      #f)

    (let ((f (case-lambda/with-class
	      (((a <fraction>) (b <complex>))
	       (list a.numerator b.magnitude)))))
      (check (f (make-<fraction> 2/3) -4) => '(2 4))
      #f)

    (let ((f (case-lambda/with-class
	      (((a <fraction>) b (c <fraction>))
	       (list a.numerator b c.denominator)))))
      (check (f (make-<fraction> 2/3) 4 (make-<fraction> 5/6)) => '(2 4 6))
      #f)

    (let ((f (case-lambda/with-class
	      (((a <fraction>) . rest)
	       (vector a.numerator rest)))))
      (check (f (make-<fraction> 11/12)) => '#(11 ()))
      (check (f (make-<fraction> 11/12) 2) => '#(11 (2)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 (2 3 4)))
      #f)

    (let ((f (case-lambda/with-class
	      (((a <fraction>) b . rest)
	       (vector a.numerator b rest)))))
      (check (f (make-<fraction> 11/12) 2) => '#(11 2 ()))
      (check (f (make-<fraction> 11/12) 2 3) => '#(11 2 (3)))
      (check (f (make-<fraction> 11/12) 2 3 4) => '#(11 2 (3 4)))
      #f)

    #f)

;;; --------------------------------------------------------------------
;;; multiple clauses

  (let ((f (case-lambda/with-class
	    ((a) a)
	    ((a b) (list a b)))))
    (check (f 1) => 1)
    (check (f 1 2) => '(1 2))
    #f)

  (let ((f (case-lambda/with-class
	    ((a)	a)
	    ((a b)	(list a b))
	    ((a b c)	(list a b c))
	    )))
    (check (f 1) => 1)
    (check (f 1 2) => '(1 2))
    (check (f 1 2 3) => '(1 2 3))
    #f)

  (let ()

    (define-class <fraction>
      (fields (mutable number))
      (virtual-fields (immutable numerator)
		      (immutable denominator)))

    (define <fraction>-numerator
      (case-lambda/with-class
       (((o <fraction>))
	(numerator o.number))))

    (define <fraction>-denominator
      (case-lambda/with-class
       (((o <fraction>))
	(denominator o.number))))

    (let ((f (case-lambda/with-class
	      (((a <fraction>))
	       a.numerator)
	      (((a <fraction>) (b <string>))
	       (list a.numerator b.length)))))
      (check (f (make-<fraction> 2/3)) => 2)
      (check (f (make-<fraction> 2/3) "ciao") => '(2 4))
      #f)

    (let ((f (case-lambda/with-class
	      (((a <fraction>))
	       a.numerator)
	      (((a <fraction>) (b <string>))
	       (list a.numerator b.length))
	      (((a <fraction>) (b <string>) (c <char>))
	       (list a.numerator b.length c.upcase)))))
      (check (f (make-<fraction> 2/3)) => 2)
      (check (f (make-<fraction> 2/3) "ciao") => '(2 4))
      (check (f (make-<fraction> 2/3) "ciao" #\a) => '(2 4 #\A))
      #f)

    #f)

;;; --------------------------------------------------------------------
;;; use the records from (records-lib)

  (check
      (let ((r (make <gamma> 1 2 3 4 5 6 7 8 9))
	    (f (case-lambda/with-class
		(((r <gamma> <beta> <alpha>))
		 (list r.a r.b r.c
		       r.d r.e r.f
		       r.g r.h r.i)))))
	(f r))
    => '(1 2 3 4 5 6 7 8 9))

  (check	;use the records from (records-lib)
      (let ((r (make <gamma> 1 2 3 4 5 6 7 8 9))
	    (s (make <beta>  10 20 30 40 50 60))
	    (f (case-lambda/with-class
		(((r <gamma> <alpha>) (s <beta> <alpha>))
		 (list r.a r.g s.a s.d)))))
	(f r s))
    => '(1 7 10 40))

  #t)


(parametrise ((check-test-name	'setf))

  (check	;alias of set!
      (let ((a 1))
	(setf a 2)
	a)
    => 2)

;;; --------------------------------------------------------------------

  (let ()

    (define-class <alpha>
      (fields (mutable a))
      (setter <alpha>-setf))

    (define/with-class (<alpha>-setf (o <alpha>) key value)
      (set! o.a (list key value)))

    (check
  	(let/with-class (((o <alpha>) (make <alpha> 1)))
  	  (setf (o 2) 3)
  	  o.a)
      => '(2 3))

    #f)

;;; --------------------------------------------------------------------

  (let ()

    (define-class <alpha>
      (fields (mutable a))
      (setter <alpha>-setf))

    (define/with-class (<alpha>-setf (o <alpha>) key0 key1 value)
      (set! o.a (list key0 key1 value)))

    (check
	(let/with-class (((o <alpha>) (make <alpha> 1)))
	  (setf (o 2 3) 4)
	  o.a)
      => '(2 3 4))

    #f)

  (let ()

    (define-class <alpha>
      (fields (mutable a))
      (setter <alpha>-setf)
      (getter <alpha>-getf))

    (define/with-class (<alpha>-setf (o <alpha>) key0 key1 value)
      (set! o.a (list key0 key1 value)))

    (define/with-class (<alpha>-getf (o <alpha>) key0 key1)
      (list o.a key0 key1))

    (check
	(let/with-class (((o <alpha>) (make <alpha> 1)))
	  (setf (o 2 3) 4)
	  (getf (o 5 6)))
      => '((2 3 4) 5 6))

    #f)

;;; --------------------------------------------------------------------
;;; builtin getter/setter

  (check	;vector
      (let/with-class (((o <vector>) (vector 0 1 2 3 4)))
	(setf (o 2) #\c)
	(getf (o 2)))
    => #\c)

  (check	;bytevector
      (let/with-class (((o <bytevector>) (bytevector-copy '#vu8(0 1 2 3))))
  	(setf (o 2) 10)
  	(getf (o 2)))
    => 10)

  (check	;bytevector
      (let/with-class (((o <bytevector>) (bytevector-copy '#vu8(0 1 2 3))))
  	(setf (o 2 s16 big) 10)
  	(getf (o 2 s16 big)))
    => 10)

  (check	;hashtable
      (let/with-class (((o <hashtable>) (make-eq-hashtable)))
  	(setf (o 'ciao) 10)
  	(list (getf (o 'ciao))
  	      (getf (o 'hello))
  	      (getf (o 'hello 'salut))))
    => '(10 #f salut))

  (check	;string
      (let/with-class (((o <string>) (string-copy "ciao")))
  	(setf (o 2) #\c)
  	(getf (o 2)))
    => #\c)

  #t)


(parametrise ((check-test-name	'virtual-methods))

  (let ()

    (define-class <alpha>
      (fields a)
      (methods the-a the-b the-c))

    (define-virtual-method <alpha> the-a
      (lambda ((o <alpha>))
	(+ 1 o.a)))

    (define-virtual-method <alpha> the-b)

    (define-virtual-method <alpha> the-c
      (lambda ((o <alpha>))
	'alpha))

    (define-class <beta>
      (inherit <alpha>)
      (methods the-b the-c))

    (define-virtual-method <beta> the-b
      ;;Provides the implementation for both <alpha> and <beta>.
      (lambda ((o <alpha>))
	(+ 10 o.a)))

    (define-virtual-method <beta> the-c
      ;;Overrides the implementation of <alpha>.
      (lambda ((o <beta>))
	'beta))

    ;; <alpha> methods

    (check
	(let (((o <alpha>) (make <alpha> 2)))
	  (o.the-a))
      => 3)

    (check
	(guard (E ((syntax-violation? E)
;;;(write (condition-message E))(newline)
		   #t)
		  (else
;;;(write (condition-message E))(newline)
                   #f))
	  (let (((o <alpha>) (make <alpha> 2)))
	    (o.the-b)))
      => #t)

    (check
	(let (((o <alpha>) (make <alpha> 2)))
	  (o.the-c))
      => 'alpha)

    ;; <beta> methods

    (check
	(let (((o <beta>) (make <beta> 2)))
	  (o.the-a))
      => 3)

    (check
	(let (((o <beta>) (make <beta> 2)))
	  (o.the-b))
      => 12)

    (check
	(let (((o <beta>) (make <beta> 2)))
	  (o.the-c))
      => 'beta)

    (check	;A  <beta> object  seen as  <alpha> object  provides the
		;virtual method implementation.
	(let (((o <alpha>) (make <beta> 2)))
	  (o.the-c))
      => 'beta)

    #f)

  #t)


(parametrise ((check-test-name 'parent-list))

;;;We cannot  rely on the  RTDs to be  equal when the definition  of the
;;;corresponding records comes from different library import paths, even
;;;when the records are nongenerative.  So we compare the UIDs.

  (let ()
    (define-class <alpha>)
    (define-class <beta>
      (inherit <alpha>))
    (define-class <gamma>
      (inherit <beta>))

    (check
	(map record-type-uid (record-parent-list (class-record-type-descriptor <alpha>)))
      => (map record-type-uid (list (class-record-type-descriptor <alpha>)
				    (class-record-type-descriptor <top>))))

    (check
	(map record-type-uid (record-parent-list (class-record-type-descriptor <beta>)))
      => (map record-type-uid (list (class-record-type-descriptor <beta>)
				    (class-record-type-descriptor <alpha>)
				    (class-record-type-descriptor <top>))))

    (check
	(map record-type-uid (record-parent-list (class-record-type-descriptor <gamma>)))
      => (map record-type-uid (list (class-record-type-descriptor <gamma>)
				    (class-record-type-descriptor <beta>)
				    (class-record-type-descriptor <alpha>)
				    (class-record-type-descriptor <top>))))
    #t)

;;; --------------------------------------------------------------------
;;; The following tests use the hierarchy from the (records-lib) library

  (let ((env (environment '(rnrs) '(classes) '(records-lib))))

    (check
	(map record-type-uid (class-parent-rtd-list <alpha>))
      => (eval '(map record-type-uid (list (class-record-type-descriptor <alpha>)
					   (class-record-type-descriptor <top>)))
	       env))

    (check
	(map record-type-uid (class-parent-rtd-list <beta>))
      => (eval '(map record-type-uid (list (class-record-type-descriptor <beta>)
					   (class-record-type-descriptor <alpha>)
					   (class-record-type-descriptor <top>)))
	       env))

    (check
	(map record-type-uid (class-parent-rtd-list <gamma>))
      => (eval '(map record-type-uid (list (class-record-type-descriptor <gamma>)
					   (class-record-type-descriptor <beta>)
					   (class-record-type-descriptor <alpha>)
					   (class-record-type-descriptor <top>)))
	       env))
    #f)

  #t)


(parametrise ((check-test-name 'uid-list))

  (let ()
    (define-class <alpha>
      (nongenerative root:<alpha>))
    (define-class <beta>
      (inherit <alpha>)
      (nongenerative root:<beta>))
    (define-class <gamma>
      (inherit <beta>)
      (nongenerative root:<gamma>))

    (check
	(class-uid-list <top>)
      => '( nausicaa:builtin:<top>))

    (check
	(class-uid-list <alpha>)
      => '(root:<alpha> nausicaa:builtin:<top>))

    (check
	(class-uid-list <beta>)
      => '(root:<beta> root:<alpha> nausicaa:builtin:<top>))

    (check
	(class-uid-list <gamma>)
      => '(root:<gamma> root:<beta> root:<alpha> nausicaa:builtin:<top>))

    #f)

;;; --------------------------------------------------------------------

  (check-for-true
   (class-uid-equal-or-parent? (class-uid-list <top>) (class-uid-list <top>)))

  (check-for-true
   (class-uid-equal-or-parent? (class-uid-list <top>) (class-uid-list <builtin>)))

  (check-for-false
   (class-uid-equal-or-parent? (class-uid-list <builtin>) (class-uid-list <top>)))

  (check-for-true
   (class-uid-equal-or-parent? (class-uid-list <number>) (class-uid-list <integer>)))

  (check-for-false
   (class-uid-equal-or-parent? (class-uid-list <integer>) (class-uid-list <number>)))

  (check-for-false
   (class-uid-equal-or-parent? (class-uid-list <fixnum>) (class-uid-list <flonum>)))

  (check-for-false
   (class-uid-equal-or-parent? (class-uid-list <flonum>) (class-uid-list <fixnum>)))

  #t)


(parametrise ((check-test-name 'macro-makers))

;;; These tests make use of the record types exported by (records-lib).

  (let ((a (make <alpha>
	     1 2 3))
	(b (make <beta>
	     1 2 3
	     4 5 6)))

    (check
    	(list ((record-accessor (class-record-type-descriptor <alpha>) 0) a)
	      ((record-accessor (class-record-type-descriptor <alpha>) 1) a)
	      ((record-accessor (class-record-type-descriptor <alpha>) 2) a))
      => '(1 2 3))

    (check
    	(list ((record-accessor (class-record-type-descriptor <alpha>) 0) b)
	      ((record-accessor (class-record-type-descriptor <alpha>) 1) b)
	      ((record-accessor (class-record-type-descriptor <alpha>) 2) b)
	      ((record-accessor (class-record-type-descriptor <beta>) 0) b)
	      ((record-accessor (class-record-type-descriptor <beta>) 1) b)
	      ((record-accessor (class-record-type-descriptor <beta>) 2) b))
      => '(1 2 3 4 5 6))

    #f)

  #t)


(parametrise ((check-test-name 'predicates))

;;; These tests make use of the record types exported by (records-lib).

  (let ((a (make <alpha>
	     1 2 3))
	(b (make <beta>
	     1 2 3
	     4 5 6)))

    (check
    	(is-a? a <alpha>)
      => #t)

    (check
    	(is-a? b <beta>)
      => #t)

    #f)

;;; --------------------------------------------------------------------
;;; Special syntax.

  (let ()

    (define-class <alpha>
      (fields a))

    (define-class <beta>
      (fields b))

    (define a (make <alpha> 1))
    (define b (make <beta>  2))

    (check	;special syntax
    	((is-a? <> <beta>) b)
      => #t)

    (check	;special syntax
    	((is-a? <> <beta>) a)
      => #f)

    (check	;special syntax
    	(for-all (is-a? <> <alpha>) (list a a a))
      => #t)

    (check	;special syntax
    	(for-all (is-a? <> <alpha>) (list a b a))
      => #f)

    #f)

;;; --------------------------------------------------------------------
;;; These tests make use of the record types exported by (records).

  (check-for-true	(is-a? 123 <fixnum>))
  (check-for-false	(is-a? #\a <fixnum>))

  (check-for-true	(is-a? 1 <integer>))
  (check-for-false	(is-a? 1.2 <integer>))

  (check-for-true	(is-a? 1/2 <rational>))
  (check-for-false	(is-a? 1+2i <rational>))

  (check-for-true	(is-a? 1.0 <integer-valued>))
  (check-for-false	(is-a? 1.1 <integer-valued>))

  (check-for-true	(is-a? 1/2 <rational-valued>))
  (check-for-false	(is-a? #\a <rational-valued>))

  (check-for-true	(is-a? 1.1 <flonum>))
  (check-for-false	(is-a? #\a <flonum>))

  (check-for-true	(is-a? 1.1 <real>))
  (check-for-false	(is-a? #\a <real>))

  (check-for-true	(is-a? 1.1 <real-valued>))
  (check-for-false	(is-a? #\a <real-valued>))

  (check-for-true	(is-a? 1.1+2i <complex>))
  (check-for-false	(is-a? #\a <complex>))

  (check-for-true	(is-a? 1 <number>))
  (check-for-false	(is-a? #\a <number>))

  (check-for-true	(is-a? #\a <char>))
  (check-for-false	(is-a? 1 <char>))

  (check-for-true	(is-a? "ciao" <string>))
  (check-for-false	(is-a? 123 <string>))

  (check-for-true	(is-a? '#(1 2 3) <vector>))
  (check-for-false	(is-a? "ciao" <vector>))

  (check-for-true	(is-a? '#vu8(1 2 3) <bytevector>))
  (check-for-false	(is-a? "ciao" <bytevector>))

  (check-for-true	(is-a? (make-eq-hashtable) <hashtable>))
  (check-for-false	(is-a? "ciao" <hashtable>))

  (check-for-true	(is-a? (open-string-input-port "ciao") <input-port>))
  (check-for-false	(is-a? 123 <input-port>))

  (check-for-true	(let-values (((port getter) (open-string-output-port)))
  			  (is-a? port <output-port>)))
  (check-for-false	(is-a? 123 <output-port>))

  ;; (check-for-true	(let-values (((port getter) (open-string-output-port)))
  ;; 			  (is-a? port <output-port>)))
  (check-for-false	(is-a? 123 <binary-port>))

  (check-for-true	(let-values (((port getter) (open-string-output-port)))
  			  (is-a? port <textual-port>)))
  (check-for-false	(is-a? 123 <textual-port>))

  (check-for-true	(is-a? (open-string-input-port "ciao") <port>))
  (check-for-false	(is-a? 123 <port>))

  (check-for-true	(is-a? (make-message-condition "ciao") <condition>))
  (check-for-false	(is-a? 123 <condition>))

  (check-for-true	(is-a? (make <alpha> 1 2 3) <record>))
  (check-for-false	(is-a? 123 <record>))

  (check-for-true	(is-a? '(1 . 2) <pair>))
  (check-for-false	(is-a? 1 <pair>))

  (check-for-true	(is-a? '(1 2) <list>))
  (check-for-false	(is-a? '(1 . 2) <list>))

  #t)


(parametrise ((check-test-name 'inspection))

  (check
      (class-type-uid <vector>)
    => 'nausicaa:builtin:<vector>)

;;; --------------------------------------------------------------------

  (let ()
    (define-class <alpha>
      (fields (mutable a)
  	      (immutable b)
  	      (mutable c)))

    (define-class <beta>
      (inherit <alpha>)
      (fields (mutable d)
  	      (immutable e)
  	      (mutable f)))

    (define-class <gamma>
      (inherit <beta>)
      (fields (mutable g)
  	      (immutable h)
  	      (mutable i)))

    (define a (make-<alpha> #f #f #f))
    (define g (make-<gamma> #f #f #f
  			    #f #f #f
  			    #f #f #f))

    (check
  	(record-type-uid (record-type-of a))
      => (record-type-uid (class-record-type-descriptor <alpha>)))

    (check
  	(record-type-uid (record-type-of g))
      => (record-type-uid (class-record-type-descriptor <gamma>)))

    #f)

;;; --------------------------------------------------------------------
;;; These tests make use of the record types exported by (records-lib).

  (let ((a (make <alpha>
  	     1 2 3))
  	(b (make <beta>
  	     1 2 3
  	     4 5 6)))

    (check
  	(record-type-uid (record-type-of a))
      => (record-type-uid (class-record-type-descriptor <alpha>)))

    (check
  	(record-type-uid (record-type-of b))
      => (record-type-uid (class-record-type-descriptor <beta>)))

    #f)

;;; --------------------------------------------------------------------
;;; These tests make use of the record types exported by (classes).

  (check
      (record-type-uid (record-type-of 123))
    => (record-type-uid (class-record-type-descriptor <fixnum>)))

  (check
      (record-type-uid (record-type-of (expt 10 30)))
    => (record-type-uid (class-record-type-descriptor <integer>)))

  (check
      (record-type-uid (record-type-of 1/2))
    => (record-type-uid (class-record-type-descriptor <rational>)))

  (check
      (record-type-uid (record-type-of #i3+0i))
    => (record-type-uid (class-record-type-descriptor <integer-valued>)))

  (check
      (record-type-uid (record-type-of #i3.0+0i))
    => (record-type-uid (class-record-type-descriptor <integer-valued>)))

  (check
      (record-type-uid (record-type-of #i3/2+0i))
    => (record-type-uid (class-record-type-descriptor <rational-valued>)))

  (check
      (record-type-uid (record-type-of #i3/2+0.0i))
    => (record-type-uid (class-record-type-descriptor <rational-valued>)))

  (check
      (record-type-uid (record-type-of #\a))
    => (record-type-uid (class-record-type-descriptor <char>)))

  (check
      (record-type-uid (record-type-of "ciao"))
    => (record-type-uid (class-record-type-descriptor <string>)))

  (check
      (record-type-uid (record-type-of '#(1 2 3)))
    => (record-type-uid (class-record-type-descriptor <vector>)))

  (check
      (record-type-uid (record-type-of '#vu8(1 2 3)))
    => (record-type-uid (class-record-type-descriptor <bytevector>)))

  (check
      (record-type-uid (record-type-of (make-eq-hashtable)))
    => (record-type-uid (class-record-type-descriptor <hashtable>)))

  (check
      (record-type-uid (record-type-of (open-string-input-port "ciao")))
    => (record-type-uid (class-record-type-descriptor <input-port>)))

  ;;It never returns <textual-port>  because input and output attributes
  ;;are checked first.
  ;; (check
  ;;     (record-type-uid (record-type-of (open-string-input-port "ciao")))
  ;;   => (record-type-uid (class-record-type-descriptor <textual-port>)))

  ;;It  never returns  <port> because  input and  output  attributes are
  ;;checked first so it returns <input-port> or <output-port>.
  ;; (check
  ;;     (record-type-uid (record-type-of (open-string-input-port "ciao")))
  ;;   => (record-type-uid (class-record-type-descriptor <port>)))

  (check
      (record-type-uid (record-type-of (current-output-port)))
    => (record-type-uid (class-record-type-descriptor <output-port>)))

  (check
      (record-type-uid (record-type-of (make-message-condition "ciao")))
    => (record-type-uid (record-type-descriptor &message)))

  (check
      (record-type-uid (record-type-of '(1 . 2)))
    => (record-type-uid (class-record-type-descriptor <pair>)))

  #t)


(parametrise ((check-test-name 'builtin))

;;; <list>

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o.find (lambda (n) (= 3 n))))
    => 3)

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o.for-all (lambda (n) (< 0 n))))
    => #t)

  (check
      (let (((o <list>) '(1 2 3 4 5)))
	(o.exists (lambda (n) (if (= 3 n) n #f))))
    => 3)

;;; --------------------------------------------------------------------

  #t)


;;;; done

(check-report)

;;; end of file

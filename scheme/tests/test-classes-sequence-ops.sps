;;; -*- coding: utf-8 -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for bindings
;;;Date: Sat Nov 13, 2010
;;;
;;;Abstract
;;;
;;;	I  stumbled on the  home page  of the  Gosu language[1]  and got
;;;	curious on this syntax for a sequence of operations upon a list:
;;;
;;;		// Declare some data
;;;		var minLength = 4
;;;		var strings = { "yellow", "red", "blue" }
;;;
;;;		// Slice and dice the data using blocks
;;;		print( strings.where( \ s -> s.length() >= minLength )
;;;		              .sort()
;;;		              .join( ", " ) )
;;;
;;;	it is possible to mimic it with the facilities of (classes).
;;;
;;;	[1] <http://gosu-lang.org/>
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
  (prefix (syntax-utilities) syn.)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing class bindings with operations sequences\n")


(let ()	;single label with operations sequences

  (define-label <l>
    (inherit <list>)
    (bindings <l>-ops))

  (define-syntax <l>-ops
    (lambda (stx)
      (syntax-case stx ()
	((_ ?class ?var . ?body)
	 (with-syntax ((VARD (syn.identifier-suffix #'?var ".")))
	   #'(let-syntax
		 ((VARD (syntax-rules ()
			  ((_ ?clause (... ...))
			   (<l>-expand-operations-sequence
			    ?var ?var ?clause (... ...))))))
	       . ?body))))))

  (define-auxiliary-syntaxes
    ;;R6RS does not allow identifiers to start with a dot.
    :filter
    :sort)

  (define-syntax <l>-expand-operations-sequence
    (syntax-rules (:filter :sort)
      ((_ ?expr ?var)
       ?expr)
      ((_ ?expr ?var (:filter ?proc) ?clause ...)
       (<l>-expand-operations-sequence
	(filter ?proc ?expr) ?var ?clause ...))
      ((_ ?expr ?var (:sort ?<) ?clause ...)
       (<l>-expand-operations-sequence
	(list-sort ?< ?expr) ?var ?clause ...))
      ))

;;; --------------------

  (let (((ell <l>) '(1 2 3 4 5)))
    (check
	(ell. (:filter odd?)
	      (:sort   >))
      => '(5 3 1))
    #f)

  #t)


(let ()	;composing operations sequences from a hierarchy of labels

  (define-label <l1>
    (inherit <list>)
    (bindings <l1>-ops))

  (define-label <l2>
    (inherit <l1>)
    (bindings <l2>-ops))

  (define-label <l3>
    (inherit <l2>)
    (bindings <l3>-ops))

  (define-syntax <l1>-ops
    (lambda (stx)
      (syntax-case stx ()
	((_ ?class ?var . ?body)
	 (with-syntax ((VARD (syn.identifier-suffix #'?var ".")))
	   #'(let-syntax
		 ((VARD (syntax-rules ()
			  ((_ ?clause (... ...))
			   (<l1>-expand-operations-sequence
			    #f #f
			    ?var ?var ?clause (... ...))))))
	       . ?body))))))

  (define-syntax <l2>-ops
    (lambda (stx)
      (syntax-case stx ()
	((_ ?class ?var . ?body)
	 (with-syntax ((VARD (syn.identifier-suffix #'?var ".")))
	   #'(let-syntax
		 ((VARD (syntax-rules ()
			  ((_ ?clause (... ...))
			   (<l2>-expand-operations-sequence
			    <l2>-expand-operations-sequence #f
			    ?var ?var ?clause (... ...))))))
	       . ?body))))))

  (define-syntax <l3>-ops
    (lambda (stx)
      (syntax-case stx ()
	((_ ?class ?var . ?body)
	 (with-syntax ((VARD (syn.identifier-suffix #'?var ".")))
	   #'(let-syntax
		 ((VARD (syntax-rules ()
			  ((_ ?clause (... ...))
			   (<l3>-expand-operations-sequence
			    <l3>-expand-operations-sequence #f
			    ?var ?var ?clause (... ...))))))
	       . ?body))))))

  (define-auxiliary-syntaxes
    ;;R6RS does not allow identifiers to start with a dot.
    :filter
    :sort
    :remv)

  (define-syntax <l1>-expand-operations-sequence
    (lambda (stx)
      (syntax-case stx (:filter)
	((_ ?kont ?loop ?expr ?var)
	 #'?expr)
	((_ ?kont ?loop ?expr ?var (:filter ?proc) ?clause ...)
	 #'(<l1>-expand-operations-sequence
	    ?kont #f (filter ?proc ?expr) ?var ?clause ...))
	((_ ?kont #f ?expr ?var (?op ?arg ...) ?clause ...)
	 (identifier? #'?kont)
	 ;;unknown operation try the given continuation
	 #'(?kont ?kont #t ?expr ?var (?op ?arg ...) ?clause ...))
	((_ ?kont #t ?expr ?var (?op ?arg ...) ?clause ...)
	 (identifier? #'?kont)
	 ;;unknown operation raise error
	 (syntax-violation #f "invalid clause" stx #'(?op ?arg ...)))
	)))

  (define-syntax <l2>-expand-operations-sequence
    (syntax-rules (:sort)
      ((_ ?kont ?loop ?expr ?var)
       ?expr)
      ((_ ?kont ?loop ?expr ?var (:sort ?<) ?clause ...)
       (<l2>-expand-operations-sequence
	?kont #f (list-sort ?< ?expr) ?var ?clause ...))
      ((_ ?kont ?loop ?expr ?var (?op ?arg ...) ?clause ...)
       ;;unknown operation: try the superlabel
       (<l1>-expand-operations-sequence
	?kont ?loop ?expr ?var (?op ?arg ...) ?clause ...))
      ))

  (define-syntax <l3>-expand-operations-sequence
    (syntax-rules (:remv)
      ((_ ?kont ?loop ?expr ?var)
       ?expr)
      ((_ ?kont ?loop ?expr ?var (:remv ?obj) ?clause ...)
       (<l3>-expand-operations-sequence
	?kont #f (remv ?obj ?expr) ?var ?clause ...))
      ((_ ?kont ?loop ?expr ?var (?op ?arg ...) ?clause ...)
       ;;unknown operation: try the superlabel
       (<l2>-expand-operations-sequence
	?kont ?loop ?expr ?var (?op ?arg ...) ?clause ...))
      ))

;;; --------------------

  (let (((ell <l1>) '(1 2 3 4 5)))

    (check
	(ell. (:filter odd?))
      => '(1 3 5))

;;;syntax violation
;;;    (ell. (:wop 123))

    #f)

  (let (((ell <l2>) '(1 2 3 4 5)))

    (check
	(ell. (:sort   >)
	      (:filter odd?))
      => '(5 3 1))

    (check
	(ell. (:filter odd?)
	      (:sort   >))
      => '(5 3 1))

;;;syntax violation
    ;; (ell. (:filter odd?)
    ;; 	  (:wop 123))

;;;syntax violation
    ;; (ell. (:sort odd?)
    ;; 	  (:wop 123))

    #f)

  (let (((ell <l3>) '(1 2 3 4 5)))
    (check ell => ell)

    (check
	(ell. (:filter	odd?)
	      (:sort	>))
      => '(5 3 1))
    (check
	(ell. (:sort	>)
	      (:filter	odd?))
      => '(5 3 1))

    (check
    	(ell. (:remv	2)
    	      (:sort	>))
      => '(5 4 3 1))
    (check
    	(ell. (:sort	>)
	      (:remv	2))
      => '(5 4 3 1))

    (check
    	(ell. (:filter	odd?)
    	      (:remv	3))
      => '(1 5))
    (check
    	(ell. (:remv	3)
	      (:filter	odd?))
      => '(1 5))

    (check
    	(ell. (:filter	odd?)
    	      (:remv	3)
    	      (:sort	>))
      => '(5 1))
    (check
    	(ell. (:remv	3)
    	      (:sort	>)
	      (:filter	odd?))
      => '(5 1))
    (check
    	(ell. (:sort	>)
	      (:filter	odd?)
	      (:remv	3))
      => '(5 1))
    (check
    	(ell. (:filter	odd?)
    	      (:sort	>)
    	      (:remv	3))
      => '(5 1))

    ;;syntax violation
    ;; (ell. (:filter odd?)
    ;; 	  (:wop    123))
    #f)
  #t)


;;;; done

(check-report)

;;; end of file

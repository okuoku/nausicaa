;;;
;;;Part of: Nausicaa/Proofs
;;;Contents: tests for syntax objects
;;;Date: Fri Nov 28, 2008
;;;Time-stamp: <2008-11-28 13:34:16 marco>
;;;
;;;Abstract
;;;
;;;	This test  file explores the wonders of  syntax objects: SYNTAX,
;;;	SYNTAX->DATUM,  DATUM->SYNTAX.   For  tests  on macros  look  in
;;;	"test-macro.sps".
;;;
;;;Copyright (c) 2008 Marco Maggi <marcomaggi@gna.org>
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



;;;; setup

(import (rnrs)
  (uriel printing)
  (uriel test)
  (srfi parameters))

(check-set-mode! 'report-failed)


;;;; shooting the breeze with syntax objects

(parameterize ((testname 'shooting-the-breeze))

  (check
      (let ()
	(define (doit stx)
	  (syntax-case stx ()
	    ((use ?arg0 ?arg1)
	     1)))
	(let ((alpha 1)
	      (beta  2)
	      (delta 3))
	  (doit (syntax (alpha beta delta)))))
    => 1)

  (check
      (let ()
	(define (doit stx)
	  (syntax-case stx ()
	    ((use ?arg0 ?arg1)
	     (syntax->datum (syntax ?arg0)))))
	(let ((alpha 1)
	      (beta  2)
	      (delta 3))
	  (doit (syntax (alpha beta delta)))))
    => 'beta)

  (check
      (let ()
	(define (doit stx)
	  (syntax-case stx ()
	    ((use ?arg0 ?arg1)
	     (list (syntax->datum (syntax use))
		   (syntax->datum (syntax ?arg0))
		   (syntax->datum (syntax ?arg1))))))
	(let ((alpha 1)
	      (beta  2)
	      (delta 3))
	  (doit (syntax (alpha beta delta)))))
    => '(alpha beta delta))

  (check
      (let ()
	(define (doit stx)
	  (syntax-case stx ()
	    ((use ?arg0 ?arg1)
	     (syntax->datum (syntax (use ?arg0 ?arg1))))))
	(let ((alpha 1)
	      (beta  2)
	      (delta 3))
	  (doit (syntax (alpha beta delta)))))
    => '(alpha beta delta))

  )



;;;; shooting the breeze with syntax objects, but inside a macro

(parameterize ((testname 'syntax-object))

  (check
      (let ()
	(define-syntax doit
	  (lambda (macro-use-stx)
	    (syntax-case macro-use-stx ()
	      ((use)
	       (datum->syntax (syntax use) '(a))))))
	(let ((a (lambda () 123)))
	  (doit)))
    => 123)

  (check
      (let ()
	(define-syntax doit
	  (lambda (macro-use-stx)
	    (syntax-case macro-use-stx ()
	      ((use)
	       (datum->syntax (syntax use) '(a))))))
	(define (alpha a)
	  (doit))
	(let ((a (lambda () 123)))
	  (alpha a)))
    => 123)

  (check
      (let ()
	(define-syntax doit
	  (lambda (macro-use-stx)
	    (syntax-case macro-use-stx ()
	      ((use)
	       (datum->syntax (syntax use)
			      '(list (a) (b) (c)))))))
	(let ((a (lambda () 123)))
	  (let ((b (lambda () 456)))
	    (let ((c (lambda () 789)))
	      (doit)))))
    => '(123 456 789))

  (check
      (let ()
	(define-syntax doit
	  (lambda (macro-use-stx)
	    (syntax-case macro-use-stx (alpha)
	      ((use ?arg)
	       (datum->syntax (syntax ?arg) '(a))))))
	(let ((a (lambda () 123)))
	  (doit alpha)))
    => 123)

  ;; (check
  ;;  (let ()
  ;;    (define-syntax doit
  ;;      (lambda (macro-use-stx)
  ;;        (syntax-case macro-use-stx (alpha)
  ;; 	 ((use ?arg)
  ;; 	  (datum->syntax (syntax alpha) '(a))))))
  ;;    (let ((a (lambda () 123)))
  ;;      (doit alpha)))
  ;;  => 123)

  ;; ------------------------------------------------------------
  ;; The following will not work because the context argument for a
  ;; DATUM->SYNTAX  has   to  be  '(syntax   <template-id>)'  where
  ;; <template-id> is

  ;; (check
  ;;  (let ()
  ;;    (define-syntax doit
  ;;      (lambda (macro-use-stx)
  ;;        (syntax-case macro-use-stx ()
  ;; 	 ((use ?arg)
  ;; 	  (datum->syntax (syntax ?arg) '(a))))))
  ;;    (let ((a (lambda () 123)))
  ;;      (doit 'alpha)))
  ;;  => 123)

  ;; ------------------------------------------------------------

  (check
      (let ()
	(define-syntax doit
	  (lambda (macro-use-stx)
	    (syntax-case macro-use-stx ()
	      ((use)
	       (with-syntax ((a-in-use-region
			      (datum->syntax (syntax use) 'a)))
		 (syntax (a-in-use-region)))))))
	(let ((a (lambda () 123)))
	  (doit)))
    => 123)

  (check
      (let ()
	(define-syntax doit
	  (lambda (macro-use-stx)
	    (syntax-case macro-use-stx ()
	      ((use)
	       (with-syntax ((a (datum->syntax (syntax use) 'a))
			     (b (datum->syntax (syntax use) 'b))
			     (c (datum->syntax (syntax use) 'c)))
		 (syntax (list (a) (b) (c))))))))

	(let ((a (lambda () 123)))
	  (let ((b (lambda () 456)))
	    (let ((c (lambda () 789)))
	      (doit)))))
    => '(123 456 789))


  )



;;;; done

(check-report)

;;; end of file

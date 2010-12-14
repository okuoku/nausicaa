;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for the interpreters library
;;;Date: Wed Jul  8, 2009
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
  (rnrs eval)
  (interps)
  (only (sentinel) sentinel?)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing interpreters\n")


(parametrise ((check-test-name	'basic))

  (check
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(+ 1 2)))
    => 3)

  (check
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(begin (+ 1 2))))
    => 3)

  (check
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(+ 1 2))
	(o.eval '(+ 1 4)))
    => 5)

;;; --------------------------------------------------------------------

  (check
      (let (((o <interp>) (make* <interp>
			    (imports: '((rnrs))))))
	(o.eval '(+ 1 2)))
    => 3)

  (check
      (let (((o <interp>) (make* <interp>
			    (imports: '((rnrs))))))
	(o.eval '(begin (+ 1 2))))
    => 3)

  (check
      (let (((o <interp>) (make* <interp>
			    (imports: '((rnrs))))))
	(o.eval '(+ 1 2))
	(o.eval '(+ 1 4)))
    => 5)

;;; --------------------------------------------------------------------
;;; returning multiple values

  (check
      (receive (a b c)
	  (let (((o <interp>) (make* <interp>
				(imports: '((rnrs))))))
	    (o.eval '(values 1 2 3)))
	(list a b c))
    => '(1 2 3))

  (check
      (let (((o <interp>) (make* <interp>
			    (imports: '((rnrs))))))
	(o.eval '(values))
	#t)
    => #t)

  #t)


(parametrise ((check-test-name	'variables))

  (check
      ;;Set variables and query their values.
      ;;
      (let* (((o <interp>) (make <interp> '((rnrs))))
	     (return-value (o.eval '(let ()
				      (define-global woppa)
				      (define-global wippa 456)
				      (set! woppa 123)
				      (list woppa wippa)))))
	(list (o.variable-ref 'woppa #f)
	      (o.variable-ref 'wippa #f)
	      return-value))
    => '(123 456 (123 456)))

  (check
      ;;Set a variable  in one EVAL, retrieve its  value in a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(let ()
		   (define-global woppa 123)
		   (values)))
	(o.eval '(begin woppa)))
    => 123)

  (check
      ;;Predefine  a variable  and retrieve  its value  in  a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.variable-set! 'woppa 123)
	(o.eval '(begin woppa)))
    => 123)

  (check
      ;;Predefine  a variable  and retrieve  its value  in  a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.variable-set! 'woppa 123)
	(o.eval 'woppa))
    => 123)

  (check
      ;;Access defined but non-initialised variable.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(let () (define-global ciao) #f))
	(sentinel? (o.eval 'ciao)))
    => #t)

;;; --------------------------------------------------------------------
;;; functions

  (check
      ;;Set a variable  in one EVAL, retrieve its  value in a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.eval '(let ()
		   (define-global (woppa a)
		     (cons 123 a))
		   #f))
	(o.eval '(woppa #\b)))
    => '(123 . #\b))

  (check
      ;;Predefine  a variable  and retrieve  its value  in  a subsequent
      ;;EVAL.
      ;;
      (let (((o <interp>) (make <interp> '((rnrs)))))
	(o.variable-set! 'woppa (lambda (a)
				  (cons 123 a)))
	(o.eval '(woppa #\b)))
    => '(123 . #\b))

;;; --------------------------------------------------------------------
;;; errors


  #t)


(parametrise ((check-test-name	'clones))

  (let (((p <interp>) (make <interp> '((rnrs)))))
    (p.eval `(let ()
	       (define-global a 1)
	       (values)))
    (p.variable-set! 'b (vector 2 3))
    (let (((q <interp>) (p.clone)))

      (check
	  (q.eval '(begin a))
	=> 1)

      (check
	  (q.eval 'b)
	=> '#(2 3))

      ;; (check
      ;; 	  (eq? (p.eval 'b) (q.eval 'b))
      ;; 	=> #t)

      (check
	  (begin
	    (p.eval '(set! a 11))
	    (q.eval 'a))
	=> 1)

      ;; (check
      ;; 	  (begin
      ;; 	    (p.eval '(vector-set! b 0 22))
      ;; 	    (q.eval 'b))
      ;; 	=> '#(22 3))

      #f)
    #f)
  #t)


;;;; done

(check-report)

;;; end of file

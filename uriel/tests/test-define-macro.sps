;;;
;;;Part of: Uriel libraries
;;;Contents: tests for Common Lisp style macros
;;;Date: Sun Nov  9, 2008
;;;
;;;Abstract
;;;
;;;
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

(import (ikarus)
  (uriel printing)
  (uriel define-macro)
  (uriel test)
  (macros-test)
  (for (macro-helpers-for-expand-time) expand))

(check-set-mode! 'report-failed)


;;;; tests for DEFINE-MACRO

;;;With no arguments.
(check
    (let ()
      (define-macro (proof)
	`(list 1 2 3))
      (proof))
  => '(1 2 3))

(check
    (let ()
      (define-macro (proof a b c)
	`(list ,a ,b ,c))
      (proof 'one 'two 'three))
  => '(one two three))

(check
    (let ()
      (define-macro (proof a b c)
	`(,a ,(+ 1 b) ,c))
      (proof list 123 'three))
  => '(124 three))

(let ()
  (define-macro (false-if-exception expr)
    `(guard (exc (else #f))
       ,expr))

  (check
      (false-if-exception (list 1 2 3))
    => '(1 2 3))

  (check
      (false-if-exception (raise 'slap))
    => #f))

(check
    (let ()
      (define-macro (proof . args)
	`(,(car args) ,(+ 1 (cadr args)) ,(caddr args)))
      (proof list 123 'three))
  => '(124 three))

(check
    (let ()
      (define-macro (proof a . args)
	`(,a ,(+ 1 (car args)) ,(cadr args)))
      (proof list 123 'three))
  => '(124 three))

(check
    (let ()
      (define-macro (proof a . args)
	(begin
	  `(,a ,(+ 1 (car args)) ,(cadr args))))
      (proof list 123 'three))
  => '(124 three))


;;; Using functions in the body of the macro.

(check
    (let ()
      (define-macro (proof a . args)
	(define (slurp arg)
	  arg)
	(slurp
	 `(,a ,(+ 1 (car args)) ,(cadr args))))
      (proof list 123 'three))
  => '(124 three))

;;;This will cause a compile time error because SLURP is available
;;;at runtime, not at macro expansion time.
;;; (check
;;;  (let ()
;;;    (define (slurp arg)
;;;      arg)
;;;    (define-macro (proof a . args)
;;;      (slurp1
;;;       `(,a ,(+ 1 (car args)) ,(cadr args))))
;;;    (proof list 123 'three))
;;;  => 'error)

;;;This makes use of a helper function from (macro-helpers-for-expand-time)
(check
    (let ()
      (define-macro (proof a . args)
	(gasp
	 `(,a ,(+ 1 (car args)) ,(cadr args))))
      (proof list 123 'three))
  => '(124 three))



;;;; tests for DEFMACRO

(check
    (let ()
      (defmacro proof (a b c)
	`(list ,a ,b ,c))
      (proof 'one 'two 'three))
  => '(one two three))

(check
    (let ()
      (defmacro proof (a b c)
	`(,a ,(+ 1 b) ,c))
      (proof list 123 'three))
  => '(124 three))

(let ()
  (defmacro false-if-exception (expr)
    `(guard (exc (else #f))
       ,expr))

  (check
      (false-if-exception (list 1 2 3))
    => '(1 2 3))

  (check
      (false-if-exception (raise 'slap))
    => #f))

(check
    (let ()
      (defmacro proof args
	`(,(car args) ,(+ 1 (cadr args)) ,(caddr args)))
      (proof list 123 'three))
  => '(124 three))

(check
    (let ()
      (defmacro proof (a . args)
	`(,a ,(+ 1 (car args)) ,(cadr args)))
      (proof list 123 'three))
  => '(124 three))



;;;; tests for macros from (macros-test).

(check
    (let ()
      (the-macro-1 'one 'two 'three))
  => '(one two three))

(check
    (let ()
      (the-macro-2 'one 'two 'three))
  => '(one two three))



;;;; done

(check-report)

;;; end of file

;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for infix library
;;;Date: Sat Aug 15, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (infix)
  (for (rename (infix syntax) (infix->prefix* ifx)) expand)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing infix\n")


(parameterise ((check-test-name 'string))

;;; integers

  (check (infix-string->sexp "1")	=> 1)
  (check (infix-string->sexp "-1")	=> -1)
  (check (infix-string->sexp "+1")	=> 1)

;;; reals

  (check (infix-string->sexp "1.1")	=> 1.1)
  (check (infix-string->sexp "-1.1")	=> -1.1)
  (check (infix-string->sexp "+1.1")	=> +1.1)
  (check (infix-string->sexp "1.1e10")	=> 1.1e10)
  (check (infix-string->sexp "1.1E10")	=> 1.1e10)
  (check (infix-string->sexp "1.1e-10")	=> 1.1e-10)
  (check (infix-string->sexp "1.1E-10")	=> 1.1e-10)
  (check (infix-string->sexp "1e10")	=> 1e10)
  (check (infix-string->sexp "1E10")	=> 1e10)
  (check (infix-string->sexp "1e-10")	=> 1e-10)
  (check (infix-string->sexp "1E-10")	=> 1e-10)

  (check (infix-string->sexp ".0")	=> 0.0)
  (check (infix-string->sexp "-.0")	=> -0.0)
  (check (infix-string->sexp "0.")	=> 0.0)

;;; complexes

  (check (infix-string->sexp "1i")	(=> =) +1i)
  (check (infix-string->sexp "-1i")	(=> =) -1i)
  (check (infix-string->sexp "+1.1i")	(=> =) +1.1i)
  (check (infix-string->sexp "-1.1i")	(=> =) -1.1i)
  (check (infix-string->sexp "+.1i")	(=> =) +0.1i)
  (check (infix-string->sexp "-.1i")	(=> =) -0.1i)

;;; nan and infinity

  (check (infix-string->sexp "+nan.0")	=> +nan.0)
  (check (infix-string->sexp "-nan.0")	=> +nan.0)
  (check (infix-string->sexp "+inf.0")	=> +inf.0)
  (check (infix-string->sexp "-inf.0")	=> -inf.0)

;;; arithmetic operators

  (check (infix-string->sexp "1+2")	=> '(+ 1 2))
  (check (infix-string->sexp "1+2+3")	=> '(+ (+ 1 2) 3))
  (check (infix-string->sexp "1+2-3")	=> '(- (+ 1 2) 3))
  (check (infix-string->sexp "1+(2+3)")	=> '(+ 1 (+ 2 3)))
  (check (infix-string->sexp "1+(2-3)")	=> '(+ 1 (- 2 3)))

  (check (infix-string->sexp "1*1")	=> '(* 1 1))
  (check (infix-string->sexp "1*2*3")	=> '(* (* 1 2) 3))
  (check (infix-string->sexp "1*2/3")	=> '(/ (* 1 2) 3))
  (check (infix-string->sexp "1*(2*3)")	=> '(* 1 (* 2 3)))
  (check (infix-string->sexp "1*(2/3)")	=> '(* 1 (/ 2 3)))

  (check (infix-string->sexp "1+2*3")	=> '(+ 1 (* 2 3)))
  (check (infix-string->sexp "1-2*3")	=> '(- 1 (* 2 3)))
  (check (infix-string->sexp "1+2/3")	=> '(+ 1 (/ 2 3)))
  (check (infix-string->sexp "1-2/3")	=> '(- 1 (/ 2 3)))

  (check (infix-string->sexp "1*2+3")	=> '(+ (* 1 2) 3))
  (check (infix-string->sexp "1*2-3")	=> '(- (* 1 2) 3))
  (check (infix-string->sexp "1/2+3")	=> '(+ (/ 1 2) 3))
  (check (infix-string->sexp "1/2-3")	=> '(- (/ 1 2) 3))

  (check (infix-string->sexp "1//3")	=> '(div 1 3))
  (check (infix-string->sexp "1%3")	=> '(mod 1 3))
  (check (infix-string->sexp "1^3")	=> '(expt 1 3))

;;; functions

  (check (infix-string->sexp "sin(1.1)")	=> '(sin 1.1))
  (check (infix-string->sexp "cos(sin(1.1))")	=> '(cos (sin 1.1)))
  (check (infix-string->sexp "cos(sin(1.1)+4)")	=> '(cos (+ (sin 1.1) 4)))
  (check (infix-string->sexp "fun(1.1, 2)")	=> '(fun 1.1 2))
  (check (infix-string->sexp "fun(1, 2, 3, 4)") => '(fun 1 2 3 4))

  (check (infix-string->sexp "fun(1+a, sin(2), 3, 4)")
    => '(fun (+ 1 a) (sin 2) 3 4))

  (check
      (infix-string->sexp "fun(1+a, sin(2), 3*g, 4+a+f+r+t)")
    => '(fun (+ 1 a) (sin 2) (* 3 g) (+ (+ (+ (+ 4 a) f) r) t)))

  (check
      (infix-string->sexp "fun(1+a, sin(2), fun(1, fun(5, 6), fun(1, 2)), 4)")
    => '(fun (+ 1 a) (sin 2) (fun 1 (fun 5 6) (fun 1 2)) 4))

  (check
      (infix-string->sexp "1+23e-45+678.9e12*(4113+23i) / sin(545) + atan(1, 2)")
    => '(+ (+ (+ 1 23e-45) (/ (* 678.9e12 (+ 4113 +23i)) (sin 545))) (atan 1 2)))

  (check (infix-string->sexp "1 < 3")	=> '(<  1 3))
  (check (infix-string->sexp "1 > 3")	=> '(>  1 3))
  (check (infix-string->sexp "1 <= 3")	=> '(<= 1 3))
  (check (infix-string->sexp "1 >= 3")	=> '(>= 1 3))
  (check (infix-string->sexp "1 = 3")	=> '(=  1 3))

;;; variables

  (check (infix-string->sexp "a * 1.1")		=> '(* a 1.1))
  (check (infix-string->sexp "(a * b) / c")	=> '(/ (* a b) c))
  (check (infix-string->sexp "a * (b / c)")	=> '(* a (/ b c)))

  (check (infix-string->sexp "cos(a) * (tan(b) / c)")
    => '(* (cos a) (/ (tan b) c)))

  #t)


(parameterise ((check-test-name 'sexp))

;;; integers

  (check (infix->prefix 1)	=> 1)
  (check (infix->prefix -1)	=> -1)
  (check (infix->prefix +1)	=> 1)

;;; reals

  (check (infix->prefix 1.1)		=> 1.1)
  (check (infix->prefix -1.1)		=> -1.1)
  (check (infix->prefix +1.1)		=> +1.1)
  (check (infix->prefix 1.1e10)		=> 1.1e10)
  (check (infix->prefix 1.1E10)		=> 1.1e10)
  (check (infix->prefix 1.1e-10)	=> 1.1e-10)
  (check (infix->prefix 1.1E-10)	=> 1.1e-10)
  (check (infix->prefix 1e10)		=> 1e10)
  (check (infix->prefix 1E10)		=> 1e10)
  (check (infix->prefix 1e-10)		=> 1e-10)
  (check (infix->prefix 1E-10)		=> 1e-10)

  (check (infix->prefix .0)	=> 0.0)
  (check (infix->prefix -.0)	=> -0.0)
  (check (infix->prefix 0.)	=> 0.0)

;;; complexes

  (check (infix->prefix +1i)	=> +1i)
  (check (infix->prefix -1i)	=> -1i)
  (check (infix->prefix +1.1i)	=> +1.1i)
  (check (infix->prefix -1.1i)	=> -1.1i)
  (check (infix->prefix +.1i)	=> +0.1i)
  (check (infix->prefix -.1i)	=> -0.1i)

;;; nan and infinity

  (check (infix->prefix +nan.0)	=> +nan.0)
  (check (infix->prefix -nan.0)	=> +nan.0)
  (check (infix->prefix +inf.0)	=> +inf.0)
  (check (infix->prefix -inf.0)	=> -inf.0)

;;; arithmetic operators

  (check (infix->prefix '(1 + 2))	=> '(+ 1 2))
  (check (infix->prefix '(1 + 2 + 3))	=> '(+ (+ 1 2) 3))
  (check (infix->prefix '(1 + 2 - 3))	=> '(- (+ 1 2) 3))
  (check (infix->prefix '(1 + (2 + 3)))	=> '(+ 1 (+ 2 3)))
  (check (infix->prefix '(1 + (2 - 3)))	=> '(+ 1 (- 2 3)))

  (check (infix->prefix '(1 * 1))	=> '(* 1 1))
  (check (infix->prefix '(1 * 2 * 3))	=> '(* (* 1 2) 3))
  (check (infix->prefix '(1 * 2 / 3))	=> '(/ (* 1 2) 3))
  (check (infix->prefix '(1 * (2 * 3)))	=> '(* 1 (* 2 3)))
  (check (infix->prefix '(1 * (2 / 3)))	=> '(* 1 (/ 2 3)))

  (check (infix->prefix '(1 + 2 * 3))	=> '(+ 1 (* 2 3)))
  (check (infix->prefix '(1 - 2 * 3))	=> '(- 1 (* 2 3)))
  (check (infix->prefix '(1 + 2 / 3))	=> '(+ 1 (/ 2 3)))
  (check (infix->prefix '(1 - 2 / 3))	=> '(- 1 (/ 2 3)))

  (check (infix->prefix '(1 * 2 + 3))	=> '(+ (* 1 2) 3))
  (check (infix->prefix '(1 * 2 - 3))	=> '(- (* 1 2) 3))
  (check (infix->prefix '(1 / 2 + 3))	=> '(+ (/ 1 2) 3))
  (check (infix->prefix '(1 / 2 - 3))	=> '(- (/ 1 2) 3))

  (check (infix->prefix '(1 // 3))	=> '(div 1 3))
  (check (infix->prefix '(1 % 3))	=> '(mod 1 3))
  (check (infix->prefix '(1 ^ 3))	=> '(expt 1 3))

;;; functions

  (check (infix->prefix '(sin (1.1)))	=> '(sin 1.1))

  (check (infix->prefix '(cos (sin (1.1))))	=> '(cos (sin 1.1)))
  (check (infix->prefix '(cos (sin (1.1) + 4)))	=> '(cos (+ (sin 1.1) 4)))
  (check (infix->prefix '(fun (1.1 2)))		=> '(fun 1.1 2))
  (check (infix->prefix '(fun (1 2 3 4)))	=> '(fun 1 2 3 4))

  (check (infix->prefix 'fun '(1 2 3 4))	=> '(fun 1 2 3 4))

  (check (infix->prefix '(fun ((1 + a) (sin ( 2 )) 3 4)))
    => '(fun (+ 1 a) (sin 2) 3 4))

  (check (infix->prefix 'fun '((1 + a) (sin ( 2 )) 3 4))
    => '(fun (+ 1 a) (sin 2) 3 4))

  (check
      (infix->prefix '(fun ((1 + a) (sin (2)) (3 * g) (4 + a + f + r + t))))
    => '(fun (+ 1 a) (sin 2) (* 3 g) (+ (+ (+ (+ 4 a) f) r) t)))

  (check
      (infix->prefix '(fun ((1 + a)
			    (sin (2))
			    (fun (1 (fun (5 6)) (fun (1 2))))
			    4)))
    => '(fun (+ 1 a)
	     (sin 2)
	     (fun 1 (fun 5 6) (fun 1 2))
	     4))

  (check
      (infix->prefix '(1 + 23e-45 + 0.06789e2 * (4113 + +23i) / (sin (0.5)) + (atan (0.1 0.2))))
    => '(+ (+ (+ 1 23e-45) (/ (* 0.06789e2 (+ 4113 +23i)) (sin 0.5))) (atan 0.1 0.2)))

  (check
      (infix->prefix 1 '+ 23e-4 '+ 0.006789e2 '* '(4113 + +23i) '/ 'sin '(0.5) '+ 'atan '(0.1 0.2))
    => '(+ (+ (+ 1 23e-4) (/ (* 0.006789e2 (+ 4113 +23i)) (sin 0.5))) (atan 0.1 0.2)))

  (check (infix->prefix '(1 < 3))	=> '(<  1 3))
  (check (infix->prefix '(1 > 3))	=> '(>  1 3))
  (check (infix->prefix '(1 <= 3))	=> '(<= 1 3))
  (check (infix->prefix '(1 >= 3))	=> '(>= 1 3))
  (check (infix->prefix '(1 = 3))	=> '(=  1 3))

;;; variables

  (check (infix->prefix '(a * 1.1))		=> '(* a 1.1))
  (check (infix->prefix '((a * b) / c))	=> '(/ (* a b) c))
  (check (infix->prefix '(a * (b / c)))	=> '(* a (/ b c)))

  (check (infix->prefix '((cos (a)) * ((tan (b)) / c)))
    => '(* (cos a) (/ (tan b) c)))

  #t)


(parameterise ((check-test-name 'syntax))

;;; integers

  (check (ifx 1)	=> 1)
  (check (ifx -1)	=> -1)
  (check (ifx +1)	=> 1)

;;; reals

  (check (ifx 1.1)	=> 1.1)
  (check (ifx -1.1)	=> -1.1)
  (check (ifx +1.1)	=> +1.1)
  (check (ifx 1.1e10)	=> 1.1e10)
  (check (ifx 1.1E10)	=> 1.1e10)
  (check (ifx 1.1e-10)	=> 1.1e-10)
  (check (ifx 1.1E-10)	=> 1.1e-10)
  (check (ifx 1e10)	=> 1e10)
  (check (ifx 1E10)	=> 1e10)
  (check (ifx 1e-10)	=> 1e-10)
  (check (ifx 1E-10)	=> 1e-10)

  (check (ifx .0)	=> 0.0)
  (check (ifx -.0)	=> -0.0)
  (check (ifx 0.)	=> 0.0)

;;; complexes

  (check (ifx +1i)	=> +1i)
  (check (ifx -1i)	=> -1i)
  (check (ifx +1.1i)	=> +1.1i)
  (check (ifx -1.1i)	=> -1.1i)
  (check (ifx +.1i)	=> +0.1i)
  (check (ifx -.1i)	=> -0.1i)

;;; nan and infinity

  (check (ifx +nan.0)	=> +nan.0)
  (check (ifx -nan.0)	=> +nan.0)
  (check (ifx +inf.0)	=> +inf.0)
  (check (ifx -inf.0)	=> -inf.0)

;;; arithmetic operators

  (check (ifx 1 + 2)		=> (+ 1 2))
  (check (ifx 1 + 2 + 3)	=> (+ (+ 1 2) 3))
  (check (ifx 1 + 2 - 3)	=> (- (+ 1 2) 3))
  (check (ifx 1 + (2 + 3))	=> (+ 1 (+ 2 3)))
  (check (ifx 1 + (2 - 3))	=> (+ 1 (- 2 3)))

  (check (ifx 1 * 1)		=> (* 1 1))
  (check (ifx 1 * 2 * 3)	=> (* (* 1 2) 3))
  (check (ifx 1 * 2 / 3)	=> (/ (* 1 2) 3))
  (check (ifx 1 * (2 * 3))	=> (* 1 (* 2 3)))
  (check (ifx 1 * (2 / 3))	=> (* 1 (/ 2 3)))

  (check (ifx 1 + 2 * 3)	=> (+ 1 (* 2 3)))
  (check (ifx 1 - 2 * 3)	=> (- 1 (* 2 3)))
  (check (ifx 1 + 2 / 3)	=> (+ 1 (/ 2 3)))
  (check (ifx 1 - 2 / 3)	=> (- 1 (/ 2 3)))

  (check (ifx 1 * 2 + 3)	=> (+ (* 1 2) 3))
  (check (ifx 1 * 2 - 3)	=> (- (* 1 2) 3))
  (check (ifx 1 / 2 + 3)	=> (+ (/ 1 2) 3))
  (check (ifx 1 / 2 - 3)	=> (- (/ 1 2) 3))

  (check (ifx 1 // 3)		=> (div 1 3))
  (check (ifx 1 % 3)		=> (mod 1 3))
  (check (ifx 1 ^ 3)		=> (expt 1 3))

;;; functions

  (check (ifx sin (1.1))		=> (sin 1.1))

  (check (ifx cos (sin (1.1)))		=> (cos (sin 1.1)))
  (check (ifx cos (sin (1.1) + 4))	=> (cos (+ (sin 1.1) 4)))

  (check
      (ifx 1 + 23e-45 + 0.006789e2 * (4.113 + +23i) / sin (0.5) + atan (0.1 0.2))
    => (+ (+ (+ 1 23e-45) (/ (* 0.006789e2 (+ 4.113 +23i)) (sin 0.5))) (atan 0.1 0.2)))

  (check (ifx 1 < 3)	=> (<  1 3))
  (check (ifx 1 > 3)	=> (>  1 3))
  (check (ifx 1 <= 3)	=> (<= 1 3))
  (check (ifx 1 >= 3)	=> (>= 1 3))
  (check (ifx 1 = 3)	=> (=  1 3))

;;; variables

  (let ((a 1) (b 2) (c 3))
    (check (ifx a * 1.1)	=> (* a 1.1))
    (check (ifx (a * b) / c)	=> (/ (* a b) c))
    (check (ifx a * (b / c))	=> (* a (/ b c)))

    (check (ifx cos (a) * tan (b) / c)
      => (/ (* (cos a) (tan b)) c)))

  #t)


;;;; done

(check-report)

;;; end of file

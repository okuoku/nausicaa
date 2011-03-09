;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: tests for standalone infix library
;;;Date: Sat Aug 15, 2009
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


#!r6rs
(import (nausicaa)
  (prefix (only (rnrs) + - * /) rnrs.)
  (nausicaa checks))

(check-set-mode! 'report-failed)
(display "*** testing standalone infix library\n")


(parameterise ((check-test-name 'syntax))

  (check (begin (infix) #f)	=> #f)

;;; integers

  (check (infix 1)	=> 1)
  (check (infix -1)	=> -1)
  (check (infix +1)	=> 1)

;;; reals

  (check (infix 1.1)		=> 1.1)
  (check (infix -1.1)		=> -1.1)
  (check (infix +1.1)		=> +1.1)
  (check (infix 1.1e10)		=> 1.1e10)
  (check (infix 1.1E10)		=> 1.1e10)
  (check (infix 1.1e-10)	=> 1.1e-10)
  (check (infix 1.1E-10)	=> 1.1e-10)
  (check (infix 1e10)		=> 1e10)
  (check (infix 1E10)		=> 1e10)
  (check (infix 1e-10)		=> 1e-10)
  (check (infix 1E-10)		=> 1e-10)

  (check (infix .0)		=> 0.0)
  (check (infix -.0)		=> -0.0)
  (check (infix 0.)		=> 0.0)

;;; complexes

  (check (infix +1i)		=> +1i)
  (check (infix -1i)		=> -1i)
  (check (infix +1.1i)		=> +1.1i)
  (check (infix -1.1i)		=> -1.1i)
  (check (infix +.1i)		=> +0.1i)
  (check (infix -.1i)		=> -0.1i)

;;; nan and infinity

  (check (infix +nan.0)		=> +nan.0)
  (check (infix -nan.0)		=> +nan.0)
  (check (infix +inf.0)		=> +inf.0)
  (check (infix -inf.0)		=> -inf.0)

;;; arithmetic operators

  (check (infix 1 + 2)		=> (+ 1 2))
  (check (infix 1 + 2 + 3)	=> (+ (+ 1 2) 3))
  (check (infix 1 + 2 - 3)	=> (- (+ 1 2) 3))
  (check (infix 1 + (2 + 3))	=> (+ 1 (+ 2 3)))
  (check (infix 1 + (2 - 3))	=> (+ 1 (- 2 3)))

  (check (infix 1 * 1)		=> (* 1 1))
  (check (infix 1 * 2 * 3)	=> (* (* 1 2) 3))
  (check (infix 1 * 2 / 3)	=> (/ (* 1 2) 3))
  (check (infix 1 * (2 * 3))	=> (* 1 (* 2 3)))
  (check (infix 1 * (2 / 3))	=> (* 1 (/ 2 3)))

  (check (infix 1 + 2 * 3)	=> (+ 1 (* 2 3)))
  (check (infix 1 - 2 * 3)	=> (- 1 (* 2 3)))
  (check (infix 1 + 2 / 3)	=> (+ 1 (/ 2 3)))
  (check (infix 1 - 2 / 3)	=> (- 1 (/ 2 3)))

  (check (infix 1 * 2 + 3)	=> (+ (* 1 2) 3))
  (check (infix 1 * 2 - 3)	=> (- (* 1 2) 3))
  (check (infix 1 / 2 + 3)	=> (+ (/ 1 2) 3))
  (check (infix 1 / 2 - 3)	=> (- (/ 1 2) 3))

  (check (infix - 2)		=> (- 2))
  (check (infix (- 2))		=> (- 2))
  (check (infix (1 + (- 2)))	=> (+ 1 (- 2)))
  (let ((a 2))
    (check (infix (- a))	=> (- 2))
    (check (infix (1 + (- a)))	=> (+ 1 (- 2)))
    #f)

  (check (infix 1 // 3)		=> (div 1 3))
  (check (infix 1 div 3)	=> (div 1 3))
  (check (infix 1 % 3)		=> (mod 1 3))
  (check (infix 10 mod 3)	=> (mod 10 3))
  (check (infix 1 ^ 3)		=> (expt 1 3))
  (check (infix 10 expt 3)	=> (expt 10 3))

  (check (infix 1 rnrs.+ 2)		=> (rnrs.+ 1 2))
  (check (infix 1 rnrs.+ 2 rnrs.+ 3)	=> (rnrs.+ (rnrs.+ 1 2) 3))
  (check (infix 1 rnrs.+ 2 rnrs.- 3)	=> (rnrs.- (rnrs.+ 1 2) 3))
  (check (infix 1 rnrs.+ (2 rnrs.+ 3))	=> (rnrs.+ 1 (rnrs.+ 2 3)))
  (check (infix 1 rnrs.+ (2 rnrs.- 3))	=> (rnrs.+ 1 (rnrs.- 2 3)))

  (check (infix 1 rnrs.* 1)		=> (rnrs.* 1 1))
  (check (infix 1 rnrs.* 2 rnrs.* 3)	=> (rnrs.* (rnrs.* 1 2) 3))
  (check (infix 1 rnrs.* 2 rnrs./ 3)	=> (rnrs./ (rnrs.* 1 2) 3))
  (check (infix 1 rnrs.* (2 rnrs.* 3))	=> (rnrs.* 1 (rnrs.* 2 3)))
  (check (infix 1 rnrs.* (2 rnrs./ 3))	=> (rnrs.* 1 (rnrs./ 2 3)))

  (check (infix 1 rnrs.+ 2 rnrs.* 3)	=> (rnrs.+ 1 (rnrs.* 2 3)))
  (check (infix 1 rnrs.- 2 rnrs.* 3)	=> (rnrs.- 1 (rnrs.* 2 3)))
  (check (infix 1 rnrs.+ 2 rnrs./ 3)	=> (rnrs.+ 1 (rnrs./ 2 3)))
  (check (infix 1 rnrs.- 2 rnrs./ 3)	=> (rnrs.- 1 (rnrs./ 2 3)))

  (check (infix 1 rnrs.* 2 rnrs.+ 3)	=> (rnrs.+ (rnrs.* 1 2) 3))
  (check (infix 1 rnrs.* 2 rnrs.- 3)	=> (rnrs.- (rnrs.* 1 2) 3))
  (check (infix 1 rnrs./ 2 rnrs.+ 3)	=> (rnrs.+ (rnrs./ 1 2) 3))
  (check (infix 1 rnrs./ 2 rnrs.- 3)	=> (rnrs.- (rnrs./ 1 2) 3))

;;; comparison operators

  (check (infix 1 < 3)		=> (<  1 3))
  (check (infix 1 > 3)		=> (>  1 3))
  (check (infix 1 <= 3)		=> (<= 1 3))
  (check (infix 1 >= 3)		=> (>= 1 3))
  (check (infix 1 = 3)		=> (=  1 3))

;;; functions

  (let ()

    (define (fun a b c)
      (+ a b c))

    (check
	(infix fun (1 2 3))
      => (fun 1 2 3))

    (check
	(infix fun(1 2 3))
      => (fun 1 2 3))

    (check
	(infix (fun (1 2 3)))
      => (fun 1 2 3))

    #f)

  (check (infix sin (1.1))		=> (sin 1.1))

  (check (infix cos (sin (1.1)))	=> (cos (sin 1.1)))
  (check (infix cos (sin (1.1) + 4))	=> (cos (+ (sin 1.1) 4)))

  (check
      (infix 1 + 23e-45 + 0.006789e2 * (4.113 + +23i) / sin (0.5) + atan (0.1 0.2))
    => (+ (+ (+ 1 23e-45) (/ (* 0.006789e2 (+ 4.113 +23i)) (sin 0.5))) (atan 0.1 0.2)))

;;; variables

  (let ((a 1) (b 2) (c 3))
    (check (infix a * 1.1)	=> (* a 1.1))
    (check (infix (a * b) / c)	=> (/ (* a b) c))
    (check (infix a * (b / c))	=> (* a (/ b c)))

    (check (infix cos (a) * tan (b) / c)
      => (/ (* (cos a) (tan b)) c))

    (check (infix (cos (a) * tan (b) / c))
      => (/ (* (cos a) (tan b)) c))

    #f)

;;; if-then-else

  (let ((a 1) (b 2) (c 3))

    (check (infix a ? b : c)	=> (if a b c))
    (check (infix (1 ? b : c))	=> (if 1 b c))
    (check (infix (a ? 1 : c))	=> (if a 1 c))
    (check (infix (a ? b : 1))	=> (if a b 1))
    (check (infix (1 ? 2 : 3))	=> (if 1 2 3))
    (check (infix #f ? 2 : 3)	=> (if #f 2 3))
    (check (infix (#f ? 2 : 3))	=> (if #f 2 3))

    (check (infix (a * (b / a ? b : c)))
      => (* a (if (/ b a) b c)))

    (check (infix (1 + a ? 2 + b : 3 + c - 4))
      => (if (+ 1 a) (+ 2 b) (- (+ 3 c) 4)))

    #f)

;;; nested prefix expressions

  (check
      (infix (begin
	       (+ 1 2)))
    => (+ 1 2))

  (check
      (infix (begin
	       (+ 1 2)
	       (+ 3 4)))
    => (+ 3 4))

  (check
      (infix (begin
	       (let ((a 3))
		 (/ a 4))))
    => (/ 3 4))

  (let ((a 3))
    (check
	(infix (begin
		 (/ a 4)))
      => (/ a 4)))

  (check (infix (begin 1) + 2 * 3)	=> (+ 1 (* 2 3)))
  (check (infix 1 - (begin 2) * 3)	=> (- 1 (* 2 3)))
  (check (infix 1 + 2 / (begin 3))	=> (+ 1 (/ 2 3)))

  (let ((a 1) (b 2) (c 3))
    (check (infix (1 + a ? (begin
			     (+ 2 b))
		     : 3 + c - 4))
      => (if (+ 1 a) (+ 2 b) (- (+ 3 c) 4)))
    #f)

  #t)


;;;; done

(check-report)

;;; end of file

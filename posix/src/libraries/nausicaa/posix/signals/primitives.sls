;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling functions for interprocess signal callouts
;;;Date: Fri Dec 18, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix signals primitives)
  (export
    signal-bub-delivered?		signal-bub-delivered*?
    signal-bub-all-delivered
    (rename (platform:signal_bub_init		signal-bub-init)
	    (platform:signal_bub_final		signal-bub-final)
	    (platform:signal_bub_acquire	signal-bub-acquire))

    signal-raise	signal-raise*
    kill		killpg
    pause
    kill*		killpg*)
  (import (rnrs)
    (nausicaa language extensions)
    (nausicaa language compensations)
    (nausicaa ffi memory)
    (nausicaa ffi cstrings)
    (nausicaa ffi errno)
    (nausicaa posix sizeof)
    (nausicaa posix typedefs)
    (prefix (nausicaa posix signals platform) platform:))


(define (signal-bub-delivered? signum)
  (not (= 0 (platform:signal_bub_delivered signum))))

(define-syntax signal-bub-delivered*?
  (syntax-rules ()
    ((_ ?sig-set)
     (signal-bub-delivered? (unix-signal->value ?sig-set)))))

(define (signal-bub-all-delivered)
  (let loop ((i   0) (ell '()))
    (if (= i NSIG)
	((enum-set-constructor (unix-signals)) ell)
      (loop (+ 1 i)
	    (if (signal-bub-delivered? i)
		(cons (value->unix-signal-symbol i) ell)
	      ell)))))

(define (signal-raise signum)
  (unless (= 0 (platform:signal_raise signum))
    (error 'signal-raise "attempt to send invalid signal number" signum)))

(define-syntax signal-raise*
  (syntax-rules ()
    ((_ ?sig-set)
     (signal-raise (unix-signal->value ?sig-set)))))

(define (kill pid signum)
  (receive (result errno)
      (platform:kill (pid->integer pid) signum)
    (if (= -1 result)
	(raise-errno-error 'kill errno (list pid signum))
      result)))

(define (killpg pid signum)
  (receive (result errno)
      (platform:killpg (pid->integer pid) signum)
    (if (= -1 result)
	(raise-errno-error 'killpg errno (list pid signum))
      result)))

(define (pause)
  (receive (result errno)
      (platform:pause)
    result))


;;;; syntax helpers

(define-syntax kill*
  (syntax-rules ()
    ((_ ?pid ?sig-set)
     (kill ?pid (unix-signal->value ?sig-set)))))

(define-syntax killpg*
  (syntax-rules ()
    ((_ ?pid ?sig-set)
     (killpg ?pid (unix-signal->value ?sig-set)))))


;;;; done

)

;;; end of file

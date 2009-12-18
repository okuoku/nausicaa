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
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (posix signals primitives)
  (export
    signal-bub-delivered?	signal-bub-all-delivered
    (rename (platform:signal_bub_init		signal-bub-init)
	    (platform:signal_bub_final		signal-bub-final)
	    (platform:signal_bub_acquire	signal-bub-acquire))

    signal-raise
    kill
    pause)
  (import (rnrs)
    (receive)
    (compensations)
    (foreign memory)
    (foreign cstrings)
    (foreign errno)
    (posix sizeof)
    (posix typedefs)
    (prefix (posix signals platform) platform:))


(define (signal-bub-delivered? signum)
  (not (= 0 (platform:signal_bub_delivered signum))))

(define (signal-bub-all-delivered)
  (let loop ((i   0) (ell '()))
    (if (= i NSIG)
	((enum-set-constructor (interprocess-signals)) ell)
      (loop (+ 1 i)
	    (if (signal-bub-delivered? i)
		(cons (interprocess-signal->symbol i) ell)
	      ell)))))

(define (signal-raise signum)
  (unless (= 0 (platform:signal_raise signum))
    (error 'signal-raise "attempt to send invalid signal number" signum)))

(define (kill pid signum)
  (receive (result errno)
      (platform:kill (pid->integer pid) signum)
    (if (= -1 result)
	(raise-errno-error 'kill errno (list pid signum))
      result)))

(define (pause)
  (receive (result errno)
      (platform:pause)
    result))


;;;; done

)

;;; end of file

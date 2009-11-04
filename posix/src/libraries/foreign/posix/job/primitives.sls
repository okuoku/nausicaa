;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface for job control functions
;;;Date: Wed Nov  4, 2009
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


(library (foreign posix job primitives)
  (export
    ctermid		setsid		getsid
    getpgrp		setpgid
    tcgetpgrp		tcsetpgrp	tcgetsid)
  (import (rnrs)
    (receive)
    (compensations)
    (foreign ffi)
    (only (foreign memory)
	  malloc-block/c)
    (only (foreign errno)
	  raise-errno-error)
    (foreign cstrings)
    (foreign posix sizeof)
    (prefix (foreign posix job platform) platform:))


(define (ctermid)
  (with-compensations
    (let ((p (malloc-block/c L_ctermid)))
      (receive (result errno)
	  (platform:ctermid p)
	(if (pointer-null? result)
	    (raise-errno-error 'ctermid errno)
	  (cstring->string p))))))


(define (setsid)
  (receive (result errno)
      (platform:setsid)
    (if (= -1 errno)
	(raise-errno-error 'setsid errno)
      result)))

(define (getsid pid)
  (receive (result errno)
      (platform:getsid pid)
    (if (= -1 errno)
	(raise-errno-error 'getsid errno pid)
      result)))

(define (getpgrp)
  (receive (result errno)
      (platform:getpgrp)
    (if (= -1 errno)
	(raise-errno-error 'getpgrp errno)
      result)))

(define (setpgid pid pgid)
  (receive (result errno)
      (platform:setpgid pid pgid)
    (if (= -1 errno)
	(raise-errno-error 'setpgid errno (list pid pgid))
      result)))


(define (tcgetpgrp fd)
  (receive (result errno)
      (platform:tcgetpgrp fd)
    (if (= -1 errno)
	(raise-errno-error 'tcgetpgrp errno fd)
      result)))

(define (tcsetpgrp fd pgid)
  (receive (result errno)
      (platform:tcsetpgrp fd pgid)
    (if (= -1 errno)
	(raise-errno-error 'tcsetpgrp errno (list fd pgid))
      result)))

(define (tcgetsid fd)
  (receive (result errno)
      (platform:tcgetsid fd)
    (if (= -1 errno)
	(raise-errno-error 'tcgetsid errno fd)
      result)))


;;;; done

)

;;; end of file

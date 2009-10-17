;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to job control functions
;;;Date: Fri Dec 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
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
(library (foreign posix job)
  (export
    ctermid	primitive-ctermid	primitive-ctermid-function
    setsid	primitive-setsid	primitive-setsid-function
    getsid	primitive-getsid	primitive-getsid-function
    getpgrp	primitive-getpgrp	primitive-getpgrp-function
    setpgid	primitive-setpgid	primitive-setpgid-function
    tcgetpgrp	primitive-tcgetpgrp	primitive-tcgetpgrp-function
    tcsetpgrp	primitive-tcsetpgrp	primitive-tcsetpgrp-function
    tcgetsid	primitive-tcgetsid	primitive-tcgetsid-function)
  (import (nausicaa)
    (foreign ffi)
    (foreign memory)
    (foreign errno)
    (foreign cstrings)
    (foreign posix sizeof)
    (foreign posix job platform)
    (compensations))

  (define dummy
    (shared-object self-shared-object))



;;;; terminal identification

(define (primitive-ctermid)
  (with-compensations
    (let ((p (malloc-block/c L_ctermid)))
      (receive (result errno)
	  (platform-ctermid p)
	(if (pointer-null? result)
	    (raise-errno-error 'primitive-ctermid errno)
	  (cstring->string p))))))

(define-primitive-parameter
  primitive-ctermid-function primitive-ctermid)

(define (ctermid)
  ((primitive-ctermid-function)))



;;;; process group

(define (primitive-setsid)
  (receive (result errno)
      (platform-setsid)
    (if (= -1 errno)
	(raise-errno-error 'setsid errno)
      result)))

(define (primitive-getsid pid)
  (receive (result errno)
      (platform-getsid pid)
    (if (= -1 errno)
	(raise-errno-error 'getsid errno pid)
      result)))

(define (primitive-getpgrp)
  (receive (result errno)
      (platform-getpgrp)
    (if (= -1 errno)
	(raise-errno-error 'getpgrp errno)
      result)))

(define (primitive-setpgid pid pgid)
  (receive (result errno)
      (platform-setpgid pid pgid)
    (if (= -1 errno)
	(raise-errno-error 'setpgid errno (list pid pgid))
      result)))

(define-primitive-parameter
  primitive-setsid-function primitive-setsid)

(define-primitive-parameter
  primitive-getsid-function primitive-getsid)

(define-primitive-parameter
  primitive-getpgrp-function primitive-getpgrp)

(define-primitive-parameter
  primitive-setpgid-function primitive-setpgid)

(define (setsid)
  ((primitive-setsid-function)))

(define (getsid pid)
  ((primitive-getsid-function) pid))

(define (getpgrp)
  ((primitive-getpgrp-function)))

(define (setpgid pid pgid)
  ((primitive-setpgid-function) pid pgid))



;;;; terminal access

(define (primitive-tcgetpgrp fd)
  (receive (result errno)
      (platform-tcgetpgrp fd)
    (if (= -1 errno)
	(raise-errno-error 'tcgetpgrp errno fd)
      result)))

(define (primitive-tcsetpgrp fd pgid)
  (receive (result errno)
      (platform-tcsetpgrp fd pgid)
    (if (= -1 errno)
	(raise-errno-error 'tcsetpgrp errno (list fd pgid))
      result)))

(define (primitive-tcgetsid fd)
  (receive (result errno)
      (platform-tcgetsid fd)
    (if (= -1 errno)
	(raise-errno-error 'tcgetsid errno fd)
      result)))

(define-primitive-parameter
  primitive-tcgetpgrp-function primitive-tcgetpgrp)

(define-primitive-parameter
  primitive-tcsetpgrp-function primitive-tcsetpgrp)

(define-primitive-parameter
  primitive-tcgetsid-function primitive-tcgetsid)

(define (tcgetpgrp fd)
  ((primitive-tcgetpgrp-function) fd))

(define (tcsetpgrp fd pgid)
  ((primitive-tcsetpgrp-function) fd pgid))

(define (tcgetsid fd)
  ((primitive-tcgetsid-function) fd))



;;;; done

)

;;; end of file

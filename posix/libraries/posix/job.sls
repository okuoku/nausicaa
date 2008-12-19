;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to job control functions
;;;Date: Fri Dec 19, 2008
;;;Time-stamp: <2008-12-19 17:40:02 marco>
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

(library (posix job)
  (export
    ctermid primitive-ctermid primitive-ctermid-function platform-ctermid

    setsid primitive-setsid primitive-setsid-function platform-setsid
    getsid primitive-getsid primitive-getsid-function platform-getsid

    getpgrp primitive-getpgrp primitive-getpgrp-function platform-getpgrp
    setpgid primitive-setpgid primitive-setpgid-function platform-setpgid

    tcgetpgrp primitive-tcgetpgrp primitive-tcgetpgrp-function platform-tcgetpgrp
    tcsetpgrp primitive-tcsetpgrp primitive-tcsetpgrp-function platform-tcsetpgrp
    tcgetsid primitive-tcgetsid primitive-tcgetsid-function platform-tcgetsid)
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (posix sizeof))

  (define dummy
    (shared-object self-shared-object))



;;;; terminal identification

(define-c-function/with-errno platform-ctermid
  (char* ctermid (char*)))

(define (primitive-ctermid)
  (with-compensations
    (let ((p (malloc-block/c L_ctermid)))
      (receive (result errno)
	  (platform-ctermid p)
	(if (pointer-null? result)
	    (raise-errno-error 'primitive-ctermid errno)
	  (cstring->string p))))))

(define primitive-ctermid-function
  (make-parameter primitive-ctermid
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-ctermid-function
	  "expected procedure as value for the PRIMITIVE-CTERMID-FUNCTION parameter"
	  func))
      func)))

(define (ctermid)
  ((primitive-ctermid-function)))



;;;; process group

(define-c-function/with-errno platform-setsid
  (pid_t setsid (void)))

(define (primitive-setsid)
  (receive (result errno)
      (platform-setsid)
    (if (= -1 errno)
	(raise-errno-error 'setsid errno)
      result)))

(define primitive-setsid-function
  (make-parameter primitive-setsid
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-setsid-function
	  "expected procedure as value for the PRIMITIVE-SETSID-FUNCTION parameter"
	  func))
      func)))

(define (setsid)
  ((primitive-setsid-function)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-getsid
  (pid_t getsid (pid_t)))

(define (primitive-getsid pid)
  (receive (result errno)
      (platform-getsid pid)
    (if (= -1 errno)
	(raise-errno-error 'getsid errno pid)
      result)))

(define primitive-getsid-function
  (make-parameter primitive-getsid
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-getsid-function
	  "expected procedure as value for the PRIMITIVE-GETSID-FUNCTION parameter"
	  func))
      func)))

(define (getsid pid)
  ((primitive-getsid-function) pid))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-getpgrp
  (pid_t getpgrp (void)))

(define (primitive-getpgrp)
  (receive (result errno)
      (platform-getpgrp)
    (if (= -1 errno)
	(raise-errno-error 'getpgrp errno)
      result)))

(define primitive-getpgrp-function
  (make-parameter primitive-getpgrp
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-getpgrp-function
	  "expected procedure as value for the PRIMITIVE-GETPGRP-FUNCTION parameter"
	  func))
      func)))

(define (getpgrp)
  ((primitive-getpgrp-function)))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-setpgid
  (int setpgid (pid_t pid_t)))

(define (primitive-setpgid pid pgid)
  (receive (result errno)
      (platform-setpgid pid pgid)
    (if (= -1 errno)
	(raise-errno-error 'setpgid errno (list pid pgid))
      result)))

(define primitive-setpgid-function
  (make-parameter primitive-setpgid
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-setpgid-function
	  "expected procedure as value for the PRIMITIVE-SETPGID-FUNCTION parameter"
	  func))
      func)))

(define (setpgid pid pgid)
  ((primitive-setpgid-function) pid pgid))



;;;; terminal access

(define-c-function/with-errno platform-tcgetpgrp
  (pid_t tcgetpgrp (int)))

(define (primitive-tcgetpgrp fd)
  (receive (result errno)
      (platform-tcgetpgrp fd)
    (if (= -1 errno)
	(raise-errno-error 'tcgetpgrp errno fd)
      result)))

(define primitive-tcgetpgrp-function
  (make-parameter primitive-tcgetpgrp
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-tcgetpgrp-function
	  "expected procedure as value for the PRIMITIVE-TCGETPGRP-FUNCTION parameter"
	  func))
      func)))

(define (tcgetpgrp fd)
  ((primitive-tcgetpgrp-function) fd))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-tcsetpgrp
  (pid_t tcsetpgrp (int pid_t)))

(define (primitive-tcsetpgrp fd pgid)
  (receive (result errno)
      (platform-tcsetpgrp fd pgid)
    (if (= -1 errno)
	(raise-errno-error 'tcsetpgrp errno (list fd pgid))
      result)))

(define primitive-tcsetpgrp-function
  (make-parameter primitive-tcsetpgrp
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-tcsetpgrp-function
	  "expected procedure as value for the PRIMITIVE-TCSETPGRP-FUNCTION parameter"
	  func))
      func)))

(define (tcsetpgrp fd pgid)
  ((primitive-tcsetpgrp-function) fd pgid))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-tcgetsid
  (pid_t tcgetsid (int)))

(define (primitive-tcgetsid fd)
  (receive (result errno)
      (platform-tcgetsid fd)
    (if (= -1 errno)
	(raise-errno-error 'tcgetsid errno fd)
      result)))

(define primitive-tcgetsid-function
  (make-parameter primitive-tcgetsid
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-tcgetsid-function
	  "expected procedure as value for the PRIMITIVE-TCGETSID-FUNCTION parameter"
	  func))
      func)))

(define (tcgetsid fd)
  ((primitive-tcgetsid-function) fd))



;;;; done

)

;;; end of file

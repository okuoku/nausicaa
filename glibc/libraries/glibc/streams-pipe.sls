;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: interface to stream pipes functions
;;;Date: Sat Dec 20, 2008
;;;Time-stamp: <2008-12-20 08:14:54 marco>
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

(library (glibc streams-pipe)
  (export
    popen primitive-popen-function primitive-popen platform-popen
    pclose primitive-pclose-function primitive-pclose platform-pclose)
  (import (r6rs)
    (uriel lang)
    (uriel foreign))


;;;; pipe to subprocess

(define-c-function/with-errno platform-popen
  (FILE* popen (char* char*)))

(define (primitive-popen command mode)
  (with-compensations
    (let ((c-command	(string->cstring/c command))
	  (c-mode		(string->cstring/c mode)))
      (receive (result errno)
	  (platform-popen c-command c-mode)
	(if (pointer-null? result)
	    (raise-errno-error 'primitive-popen errno
			       (list command mode))
	  result)))))

(define primitive-popen-function
  (make-parameter primitive-popen
    (lambda (func)
      (if (procedure? func)
	  func
	(assertion-violation 'primitive-popen-function
	  "expected procedure as value for the PRIMITIVE-POPEN-FUNCTION parameter"
	  func)))))

(define (popen command mode)
  ((primitive-popen-function) command mode))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-pclose
  (int pclose (FILE*)))

(define (primitive-pclose stream)
  (receive (result errno)
      (platform-pclose stream)
    (unless (= 0 result)
      (raise-errno-error 'primitive-pclose errno stream))
    result))

(define primitive-pclose-function
  (make-parameter primitive-pclose
    (lambda (func)
      (if (procedure? func)
	  func
	(assertion-violation 'primitive-pclose-function
	  "expected procedure as value for the PRIMITIVE-PCLOSE-FUNCTION parameter"
	  func)))))

(define (pclose stream)
  ((primitive-pclose-function) stream))





;;;; done

)

;;; end of file

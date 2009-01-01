;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to POSIX functions for R6RS Scheme
;;;Date: Mon Nov 24, 2008
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


;;;; setup

(library (posix working-directory)
  (export
    getcwd	primitive-getcwd	primitive-getcwd-function
    chdir	primitive-chdir		primitive-chdir-function

    (rename (getcwd pwd)))
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (posix sizeof)
    (posix working-directory platform))

  (define dummy
    (shared-object self-shared-object))


;;;; code

(define (primitive-getcwd)
  (let loop ((buflen 1024))
    (with-compensations
      (let ((buffer (malloc-block/c buflen)))
	(receive (cstr errno)
	    (platform-getcwd buffer buflen)
	  (if (and (= 0 (pointer->integer cstr))
		   (or (= EINVAL errno)
		       (= ERANGE errno)))
	      (loop (* 2 buflen))
	    (begin
	      (when (pointer-null? cstr)
		(raise-errno-error 'primitive-getcwd errno))
	      (cstring->string buffer))))))))

(define (primitive-chdir directory-pathname)
  (with-compensations
    (receive (result errno)
	(platform-chdir (string->cstring/c directory-pathname))
      (unless (= 0 result)
	(raise-errno-error 'primitive-chdir errno
			   directory-pathname))
      result)))

(define-primitive-parameter
  primitive-getcwd-function primitive-getcwd)

(define-primitive-parameter
  primitive-chdir-function primitive-chdir)

(define (getcwd)
  ((primitive-getcwd-function)))

(define (chdir pathname)
  ((primitive-chdir-function) pathname))


;;;; done

)

;;; end of file

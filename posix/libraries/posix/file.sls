;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: file system functions
;;;Date: Thu Jan  1, 2009
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



;;;; setup

(library (posix file)
  (export

    ;; working directory
    getcwd	primitive-getcwd	primitive-getcwd-function
    chdir	primitive-chdir		primitive-chdir-function
    fchdir	primitive-fchdir	primitive-fchdir-function
    (rename (getcwd pwd))

    )
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (posix sizeof)
    (posix file platform))

  (define dummy
    (shared-object self-shared-object))



;;;; working directory

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

(define (primitive-fchdir fd)
  (receive (result errno)
      (platform-fchdir fd)
    (unless (= 0 result)
      (raise-errno-error 'primitive-fchdir errno fd))
    result))

(define-primitive-parameter
  primitive-getcwd-function primitive-getcwd)

(define-primitive-parameter
  primitive-chdir-function primitive-chdir)

(define-primitive-parameter
  primitive-fchdir-function primitive-fchdir)

(define (getcwd)
  ((primitive-getcwd-function)))

(define (chdir pathname)
  ((primitive-chdir-function) pathname))

(define (fchdir fd)
  ((primitive-fchdir-function) fd))



;;;; done

)

;;; end of file





;;;; done

)

;;; end of file

;;;
;;;Part of: Nausicaa/Glibc
;;;Contents: interface to file functions
;;;Date: Sat Jan  3, 2009
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

(library (name)
  (export a)
  (import (r6rs))


;;;; temporary files

(define (primitive-tempnam directory prefix)
  (with-compensations
    (let ((p	(platform-tmpnam (string->cstring/c directory)
				 (string->cstring/c prefix))))
      (begin0
	  (cstring->string p)
	(primitive-free p)))))

(define (primitive-mkdtemp template)
  (with-compensations
    (let ((p	(string->cstring/c template)))
      (receive (result errno)
	  (platform-mkdtemp p)
	(when (pointer-null? result)
	  (raise-errno-error 'primitive-mkdtemp errno template))
	(cstring->string p)))))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-tempnam-function primitive-tempnam)

(define-primitive-parameter
  primitive-mkdtemp-function primitive-mkdtemp)

;;; --------------------------------------------------------------------

(define (tempnam directory prefix)
  ((primitive-tempnam-function) directory prefix))

(define (mkdtemp template)
  ((primitive-mkdtemp-function) template))



;;;; done

)

;;; end of file

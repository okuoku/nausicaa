;;;
;;;Part of: Nausicaa/POSIX
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


(library (foreign glibc file)
  (export
    ;; temporary files
    mktemp			mktemp-function
    mkstemp			mkstemp-function
    mkdtemp			mkdtemp-function
    tmpfile			tmpfile-function
    tempnam			tempnam-function
    tmpnam			tmpnam-function

    ;; times
    lutimes			lutimes-function
    futimes			futimes-function)
  (import (except (rnrs)
		  remove truncate)
    (foreign posix helpers)
    (prefix (foreign glibc file primitives) primitive:))


;;;; temporary files

(define-parametrised mktemp template)
(define-parametrised mkstemp template)
(define-parametrised mkdtemp template)
(define-parametrised tmpfile)
(define-parametrised tempnam directory prefix)
(define-parametrised tmpnam)



;;;; file times

(define-primitive-parameter lutimes-function	primitive:lutimes)
(define-primitive-parameter futimes-function	primitive:futimes)

(define lutimes
  (case-lambda
   ((pathname access-time-sec access-time-usec modification-time-sec modification-time-usec)
    ((lutimes-function) pathname access-time-sec access-time-usec
     modification-time-sec modification-time-usec))
   ((pathname)
    ((lutimes-function) pathname))))

(define futimes
  (case-lambda
   ((fd access-time-sec access-time-usec modification-time-sec modification-time-usec)
    ((futimes-function) fd access-time-sec access-time-usec modification-time-sec modification-time-usec))
   ((fd)
    ((futimes-function) fd))))


;;;; done

)

;;; end of file

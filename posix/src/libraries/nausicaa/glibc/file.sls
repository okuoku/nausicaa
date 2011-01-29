;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to file functions
;;;Date: Sat Jan  3, 2009
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


(library (nausicaa glibc file)
  (export

    ;; directory access
    scandir			scandir-function
    (rename (primitive:make-scandir-selector-callback	make-scandir-selector-callback)
	    (primitive:make-scandir-compare-callback	make-scandir-compare-callback))

    ;; temporary files
    mktemp			mktemp-function
    tempnam			tempnam-function
    tmpnam			tmpnam-function
    tmpfile			tmpfile-function

    ;; times
    lutimes			lutimes-function
    futimes			futimes-function)
  (import (except (rnrs) remove truncate)
    (nausicaa posix helpers)
    (prefix (nausicaa glibc file primitives) primitive:))


;;;; directory access

(define-parametrised scandir dir-pathname selector-callback cmp-callback)

;;;; temporary files

(define-parametrised mktemp template)
(define-parametrised tempnam directory prefix)
(define-parametrised tmpnam)
(define-parametrised tmpfile)

;;;; file times

(define-parametrised lutimes
  ((pathname access-time-sec access-time-usec modification-time-sec modification-time-usec)
   (pathname)))

(define-parametrised futimes
  ((fd access-time-sec access-time-usec modification-time-sec modification-time-usec)
   (fd)))


;;;; done

)

;;; end of file

;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to the stat functions
;;;Date: Fri Jan  2, 2009
;;;
;;;Abstract
;;;
;;;	This is an interface  to "stat()", "fstat()" and "lstat()" which
;;;	makes use of the  stubs functions in "libnausicaa-posix.so" from
;;;	the  Nausicaa/Stubs project.
;;;
;;;	  It is a misfortune that a stubs library is needed but invoking
;;;	"dlsym()"  on  the stat  functions  fails  with  all the  Scheme
;;;	implementations.  If someone has a solution: email me!!!
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

(library (posix file stat)
  (export
    platform-stat
    platform-lstat
    platform-fstat

    stat	primitive-stat		primitive-stat-function
    lstat	primitive-lstat		primitive-lstat-function
    fstat	primitive-fstat		primitive-fstat-function
    )
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (posix sizeof))

  (define stub-lib
    (let ((o (open-shared-object 'libnausicaa-posix.so)))
      (shared-object o)
      o))


;;;; stat record




;;;; code

(define-c-function/with-errno platform-stat
  (int nausicaa_posix_stat (char* pointer)))

(define-c-function/with-errno platform-fstat
  (int nausicaa_posix_fstat (int pointer)))

(define-c-function/with-errno platform-lstat
  (int nausicaa_posix_lstat (int pointer)))

;;; --------------------------------------------------------------------

(define (real-primitive-stat func funcname pathname)
  (with-compensations
    (let ((*stat-struct	(malloc-block/c sizeof-struct-stat)))
      (receive (result errno)
	  (func (string->cstring/c pathname) *stat-struct)
	(when (= -1 result)
	  (raise-errno-error funcname errno pathname))
	result))))

(define (primitive-stat pathname)
  (real-primitive-stat platform-stat 'primitive-stat pathname))

(define (primitive-fstat pathname)
  (real-primitive-stat platform-fstat 'primitive-fstat pathname))

(define (primitive-lstat pathname)
  (real-primitive-stat platform-lstat 'primitive-lstat pathname))

;;; --------------------------------------------------------------------

(define-primitive-parameter
  primitive-stat-function primitive-stat)

(define-primitive-parameter
  primitive-fstat-function primitive-fstat)

(define-primitive-parameter
  primitive-lstat-function primitive-lstat)

;;; --------------------------------------------------------------------

(define (stat pathname)
  ((primitive-stat-function) pathname))

(define (lstat pathname)
  ((primitive-lstat-function) pathname))

(define (fstat pathname)
  ((primitive-fstat-function) pathname))



;;;; done

)

;;; end of file

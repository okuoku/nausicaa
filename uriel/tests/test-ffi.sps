;;;
;;;Part of: Uriel libraries
;;;Contents: tests for ffi library
;;;Date: Tue Nov 18, 2008
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

(import (r6rs)
  (uriel lang)
  (uriel foreign)
  (uriel test))

(check-set-mode! 'report-failed)


;;;; foreign functions

(define d (shared-object self-shared-object))

;;; --------------------------------------------------------------------
;;; This is used to raise a ENOENT errno error.

(define-c-function/with-errno primitive-chdir
  (int chdir (char*)))

(define (chdir directory-pathname)
  (with-compensations
    (receive (result errno)
	(primitive-chdir (string->cstring/c directory-pathname))
      (unless (= 0 result)
	(raise-errno-error 'chdir errno directory-pathname))
      result)))

;;; --------------------------------------------------------------------
;;; This is used to raise a EINVAL errno error.

(define-c-function/with-errno platform-pread
  (int pread (int void* int int)))

(define-syntax temp-failure-retry-minus-one
  (syntax-rules ()
    ((_ ?funcname (?primitive ?arg ...) ?irritants)
     (let loop ()
       (receive (result errno)
	   (?primitive ?arg ...)
	 (when (= -1 result)
	   (when (= EINTR errno)
	     (loop))
	   (raise-errno-error (quote ?funcname) errno ?irritants))
	 result)))))

(define-syntax do-pread-or-pwrite
  (syntax-rules ()
    ((_ ?funcname ?primitive ?fd ?pointer ?number-of-bytes ?offset)
     (temp-failure-retry-minus-one
      ?funcname
      (?primitive ?fd ?pointer ?number-of-bytes ?offset)
      ?fd))))

(define (primitive-pread fd pointer number-of-bytes offset)
  (do-pread-or-pwrite primitive-pread
		      platform-pread fd pointer number-of-bytes offset))

;;; --------------------------------------------------------------------


;;;; errno conditions

(check
    (let ((dirname '/scrappy/dappy/doo))
      (guard (exc (else
		   (list (errno-condition? exc)
			 (condition-who exc)
			 (errno-symbolic-value exc))))
	(chdir dirname)))
  => '(#t chdir ENOENT))

(check
    (guard (exc (else
;; 		 (write exc)(newline)
;; 		 (write (condition-who exc))(newline)
;; 		 (write (condition-message exc))(newline)
		 (list (errno-condition? exc)
		       (condition-who exc)
		       (errno-symbolic-value exc)
		       )))
      (primitive-pread 0 (integer->pointer 1234) 10 -10))
  => '(#t primitive-pread EINVAL))




;;;; done

(check-report)

;;; end of file

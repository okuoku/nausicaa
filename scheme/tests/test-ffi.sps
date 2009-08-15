;;;
;;;Part of: Nausicaa/Sceme
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


(import (nausicaa)
  (foreign)
  (checks)
  (compensations))

(check-set-mode! 'report-failed)
(display "*** testing ffi\n")


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
;;; This is used to raise a ENOEXEC errno error.

(define-c-function/with-errno platform-execv
  (int execv (char* pointer)))

(define (primitive-execv pathname args)
  (with-compensations
    (receive (result errno)
	(platform-execv (string->cstring/c pathname)
			(strings->argv args malloc-block/c))
      (when (= -1 result)
	(raise-errno-error 'primitive-execv errno (list pathname args))))))

(define primitive-execv-function
  (make-parameter primitive-execv
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-execv-function
	  "expected procedure as value for the PRIMITIVE-EXECV-FUNCTION parameter"
	  func))
      func)))

(define (execv pathname args)
  ((primitive-execv-function) pathname args))

;;; --------------------------------------------------------------------
;;; This is used to raise a ENOTDIR errno error.

(define-c-function/with-errno platform-opendir
  (pointer opendir (char*)))

(define (primitive-opendir pathname)
  (with-compensations
    (receive (result errno)
	(platform-opendir (string->cstring/c pathname))
      (when (pointer-null? result)
	(raise-errno-error 'primitive-opendir errno pathname))
      result)))

(define primitive-opendir-function
  (make-parameter primitive-opendir
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-opendir-function
	  "expected procedure as value for the PRIMITIVE-OPENDIR-FUNCTION parameter"
	  func))
      func)))

(define (opendir pathname)
  ((primitive-opendir-function) pathname))



(parameterize ((check-test-name	'conditions)
	       (debugging	#t))

  (check
      (let ((dirname '/scrappy/dappy/doo))
	(guard (exc (else
		     ;;(debug-print-condition "condition" exc)
		     (list (errno-condition? exc)
			   (condition-who exc)
			   (errno-symbolic-value exc))))
	  (chdir dirname)))
    => '(#t chdir ENOENT))

  (check
      (guard (exc (else
		   (list (errno-condition? exc)
			 (condition-who exc)
			 (errno-symbolic-value exc)
			 )))
	(primitive-pread 0 (integer->pointer 1234) 10 -10))
    => '(#t primitive-pread EINVAL))

  (check
      (let ((pathname '/etc/passwd))
	(guard (exc (else
		     (list (errno-condition? exc)
			   (condition-who exc)
			   (errno-symbolic-value exc)
			   )))
	  (execv pathname '())))
    => '(#t primitive-execv EACCES))

  (check
      (let ((pathname '/etc/passwd))
	(guard (exc (else
		     (list (errno-condition? exc)
			   (condition-who exc)
			   (errno-symbolic-value exc)
			   )))
	  (opendir pathname)))
    => '(#t primitive-opendir ENOTDIR))

  )



;;;WARNING:  these do not  work because  what is  returned by  ERRNO for
;;;Ypsilon and  Ikarus is  the value  of the last  "errno" saved  in the
;;;internal state variable.

;; (parameterize ((check-test-name	'errno)
;; 	       (debugging	#t))

;;   (check
;;       (begin
;; 	(errno 0)
;; 	(errno))
;;     => 0)

;;   (check
;;       (begin
;; 	(errno EINVAL)
;; 	(errno))
;;     => EINVAL)

;;   (check
;;       (begin
;; 	(errno ENOMEM)
;; 	(errno))
;;     => ENOMEM)

;;   (check
;;       (begin
;; 	(errno 0)
;; 	(errno))
;;     => 0)

;;   )


;;;; done

(check-report)

;;; end of file

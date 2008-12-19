;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to process related POSIX functions
;;;Date: Fri Dec 19, 2008
;;;Time-stamp: <2008-12-19 13:46:48 marco>
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

(library (posix process)
  (export

    ;; identification
    getpid getppid

    ;; forking
    fork primitive-fork primitive-fork-function

    ;; executing
    execv primitive-execv primitive-execv-function platform-execv
    execve primitive-execve primitive-execve-function platform-execve
    execvp primitive-execvp primitive-execvp-function platform-execvp

    ;; waiting
    platform-waitpid primitive-waitpid
    waitpid waitpid/any waitpid/any-my-group waitpid/group
    )
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (posix sizeof))

  (define dummy
    (shared-object self-shared-object))


;;;; process id

(define-c-function getpid
  (pid_t getpid (void)))

(define-c-function getppid
  (pid_t getppid (void)))



;;;; forking

(define-c-function/with-errno primitive-fork
  (pid_t fork (void)))

(define primitive-fork-function
  (make-parameter primitive-fork
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-fork-function
	  "expected procedure as value for the PRIMITIVE-FORK-FUNCTION parameter"
	  func))
      func)))

(define (fork)
  (receive (result errno)
      ((primitive-fork-function))
    (when (= -1 result)
      (raise-errno-error 'fork errno))
    result))



;;;; executing

(define-c-function/with-errno platform-execv
  (int execv (char* pointer)))

(define (primitive-execv pathname args)
  (with-compensations
    (platform-execv (string->cstring/c pathname)
		    (strings->argv args malloc-block/c))))

(define primitive-execv-function
  (make-parameter primitive-execv
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-execv-function
	  "expected procedure as value for the PRIMITIVE-EXECV-FUNCTION parameter"
	  func))
      func)))

(define (execv pathname args)
  (receive (result errno)
      ((primitive-execv-function) pathname args)
    (when (= -1 result)
      (raise-errno-error 'execv errno (list pathname args)))))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-execve
  (int execve (char* pointer pointer)))

(define (primitive-execve pathname args envs)
  (with-compensations
    (platform-execve (string->cstring/c pathname)
		     (strings->argv args malloc-block/c)
		     (strings->argv envs malloc-block/c))))

(define primitive-execve-function
  (make-parameter primitive-execve
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-execve-function
	  "expected procedure as value for the PRIMITIVE-EXECVE-FUNCTION parameter"
	  func))
      func)))

(define (execve pathname args envs)
  (receive (result errno)
      ((primitive-execve-function) pathname args envs)
    (when (= -1 result)
      (raise-errno-error 'execve errno (list pathname args envs)))))

;;; --------------------------------------------------------------------

(define-c-function/with-errno platform-execvp
  (int execvp (char* pointer)))

(define (primitive-execvp pathname args)
  (with-compensations
    (platform-execvp (string->cstring/c pathname)
		     (strings->argv args malloc-block/c))))

(define primitive-execvp-function
  (make-parameter primitive-execvp
    (lambda (func)
      (unless (procedure? func)
	(assertion-violation 'primitive-execvp-function
	  "expected procedure as value for the PRIMITIVE-EXECVP-FUNCTION parameter"
	  func))
      func)))

(define (execvp pathname args)
  (receive (result errno)
      ((primitive-execvp-function) pathname args)
    (when (= -1 result)
      (raise-errno-error 'execvp errno (list pathname args)))))



;;;; waiting

(define-c-function/with-errno platform-waitpid
  (pid_t waitpid (pid_t pointer int)))


(define (primitive-waitpid pid options)
  (with-compensations
    (let ((status* (malloc-small/c)))
      (let loop ()
	(receive (result errno)
	    (platform-waitpid pid status* options)
	  (when (= -1 result)
	    (when (= EINTR errno)
	      (loop))
	    (raise-errno-error 'waitpid errno pid))
	  (values result (peek-signed-int status* 0)))))))

(define (waitpid pid options)
  (unless (< 0 pid)
    (assertion-violation 'waitpid
      "expected strictly positive process id" pid))
  (primitive-waitpid pid options))

(define (waitpid/any options)
  (primitive-waitpid -1 options))

(define (waitpid/any-my-group options)
  (primitive-waitpid 0 options))

(define (waitpid/group gpid options)
  (unless (< 0 gpid)
    (assertion-violation 'waitpid/group
      "expected strictly positive process group id" gpid))
  (primitive-waitpid (- gpid) options))




;;;; done

)

;;; end of file

;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to process related POSIX functions
;;;Date: Fri Dec 19, 2008
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

(library (posix process)
  (export

    ;; identification
    getpid	getppid

    ;; forking
    fork	primitive-fork		primitive-fork-function

    ;; executing
    execv	primitive-execv		primitive-execv-function
    execve	primitive-execve	primitive-execve-function
    execvp	primitive-execvp	primitive-execvp-function

    ;; waiting
    waitpid	primitive-waitpid	primitive-waitpid-function
    waitpid/any
    waitpid/any-my-group
    waitpid/group)
  (import (r6rs)
    (uriel lang)
    (uriel foreign)
    (posix sizeof)
    (posix process platform))

  (define dummy
    (shared-object self-shared-object))


;;;; process id

(define (getpid)
  (platform-getpid))

(define (getppid)
  (platform-getppid))


;;;; forking

(define (primitive-fork)
  (receive (result errno)
      (platform-fork)
    (when (= -1 result)
      (raise-errno-error 'primitive-fork errno))
    result))

(define-primitive-parameter
  primitive-fork-function primitive-fork)

(define (fork)
  ((primitive-fork-function)))


;;;; executing

(define (primitive-execv pathname args)
  (with-compensations
    (receive (result errno)
	(platform-execv (string->cstring/c pathname)
			(strings->argv args malloc-block/c))
      (when (= -1 result)
	(raise-errno-error 'primitive-execv errno
			   (list pathname args))))))

(define (primitive-execve pathname args envs)
  (with-compensations
    (receive (result errno)
	(platform-execve (string->cstring/c pathname)
			 (strings->argv args malloc-block/c)
			 (strings->argv envs malloc-block/c))
      (when (= -1 result)
	(raise-errno-error 'primitive-execve errno
			   (list pathname args envs))))))

(define (primitive-execvp pathname args)
  (with-compensations
    (receive (result errno)
	(platform-execvp (string->cstring/c pathname)
			 (strings->argv args malloc-block/c))
      (when (= -1 result)
	(raise-errno-error 'primitive-execvp errno
			   (list pathname args))))))

(define-primitive-parameter
  primitive-execv-function primitive-execv)

(define-primitive-parameter
  primitive-execve-function primitive-execve)

(define-primitive-parameter
  primitive-execvp-function primitive-execvp)

(define (execv pathname args)
  ((primitive-execv-function) pathname args))

(define (execve pathname args envs)
  ((primitive-execve-function) pathname args envs))

(define (execvp pathname args)
  ((primitive-execvp-function) pathname args))



;;;; waiting

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

(define-primitive-parameter
  primitive-waitpid-function primitive-waitpid)

(define (waitpid pid options)
  (unless (< 0 pid)
    (assertion-violation 'waitpid
      "expected strictly positive process id" pid))
  ((primitive-waitpid-function) pid options))

(define (waitpid/any options)
  ((primitive-waitpid-function) -1 options))

(define (waitpid/any-my-group options)
  ((primitive-waitpid-function) 0 options))

(define (waitpid/group gpid options)
  (unless (< 0 gpid)
    (assertion-violation 'waitpid/group
      "expected strictly positive process group id" gpid))
  ((primitive-waitpid-function) (- gpid) options))



;;;; done

)

;;; end of file

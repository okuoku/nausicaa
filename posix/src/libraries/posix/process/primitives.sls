;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: marshaling interface to process functions
;;;Date: Wed Nov  4, 2009
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


(library (posix process primitives)
  (export

    (rename (platform:getpid		getpid)
	    (platform:getppid		getppid))

    fork		execv
    execve		execvp
    system		waitpid)
  (import (rnrs)
    (receive)
    (compensations)
    (only (foreign memory) malloc-small/c malloc-block/c)
    (only (foreign errno) raise-errno-error EINTR)
    (only (foreign cstrings) string->cstring/c strings->argv)
    (only (foreign ffi peekers-and-pokers) pointer-ref-c-uint8)
    (prefix (posix process platform) platform:))


;;;; forking

(define (fork)
  (receive (result errno)
      (platform:fork)
    (when (= -1 result)
      (raise-errno-error 'fork errno))
    result))


;;;; executing

(define (execv pathname args)
  (with-compensations
    (receive (result errno)
	(platform:execv (string->cstring/c pathname)
			(strings->argv args malloc-block/c))
      (when (= -1 result)
	(raise-errno-error 'execv errno (list pathname args))))))

(define (execve pathname args envs)
  (with-compensations
    (receive (result errno)
	(platform:execve (string->cstring/c pathname)
			 (strings->argv args malloc-block/c)
			 (strings->argv envs malloc-block/c))
      (when (= -1 result)
	(raise-errno-error 'execve errno (list pathname args envs))))))

(define (execvp pathname args)
  (with-compensations
    (receive (result errno)
	(platform:execvp (string->cstring/c pathname)
			 (strings->argv args malloc-block/c))
      (when (= -1 result)
	(raise-errno-error 'execvp errno (list pathname args))))))

(define (system command)
  (with-compensations
    (receive (result errno)
	(platform:system (string->cstring/c command))
      (when (= -1 result)
	(raise-errno-error 'system errno command))
      result)))


;;;; waiting

(define (waitpid pid options)
  (with-compensations
    (let ((status* (malloc-small/c)))
      (let loop ()
	(receive (result errno)
	    (platform:waitpid pid status* options)
	  (when (= -1 result)
	    (when (= EINTR errno)
	      (loop))
	    (raise-errno-error 'waitpid errno pid))
	  (values result (pointer-ref-c-uint8 status* 0)))))))


;;;; done

)

;;; end of file

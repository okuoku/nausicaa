;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to platform functions for process execution
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


#!r6rs
(library (foreign posix process platform)
  (export
    platform-getpid
    platform-getppid
    platform-fork
    platform-execv
    platform-execve
    platform-execvp
    platform-system
    platform-waitpid)
  (import (rnrs)
    (foreign ffi)
    (foreign posix sizeof))


;;;; code

(define-c-function platform-getpid
  (pid_t getpid (void)))

(define-c-function platform-getppid
  (pid_t getppid (void)))

(define-c-function/with-errno platform-fork
  (pid_t fork (void)))

(define-c-function/with-errno platform-execv
  (int execv (char* pointer)))

(define-c-function/with-errno platform-execve
  (int execve (char* pointer pointer)))

(define-c-function/with-errno platform-execvp
  (int execvp (char* pointer)))

(define-c-function/with-errno platform-system
  (int system (char*)))

(define-c-function/with-errno platform-waitpid
  (pid_t waitpid (pid_t pointer int)))



;;;; done

)

;;; end of file

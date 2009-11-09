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


(library (foreign posix process platform)
  (export
    getpid		getppid
    fork		execv
    execve		execvp
    system		waitpid

    WIFEXITED		WEXITSTATUS
    WIFSIGNALED		WTERMSIG
    WCOREDUMP		WIFSTOPPED
    WSTOPSIG)
  (import (rnrs)
    (parameters)
    (foreign posix shared-object)
    (foreign ffi)
    (foreign posix sizeof))


(define dummy
  (shared-object standard-c-library))

(define-c-function getpid
  (pid_t getpid (void)))

(define-c-function getppid
  (pid_t getppid (void)))

(define-c-function/with-errno fork
  (pid_t fork (void)))

(define-c-function/with-errno execv
  (int execv (char* pointer)))

(define-c-function/with-errno execve
  (int execve (char* pointer pointer)))

(define-c-function/with-errno execvp
  (int execvp (char* pointer)))

(define-c-function/with-errno system
  (int system (char*)))

(define-c-function/with-errno waitpid
  (pid_t waitpid (pid_t pointer int)))


(define dummy2
  (shared-object libnausicaa-posix))

(define-c-function WIFEXITED
  (int nausicaa_posix_wifexited	(int)))

(define-c-function WEXITSTATUS
  (int nausicaa_posix_wexitstatus (int)))

(define-c-function WIFSIGNALED
  (int nausicaa_posix_wifsignaled (int)))

(define-c-function WTERMSIG
  (int nausicaa_posix_wtermsig (int)))

(define-c-function WCOREDUMP
  (int nausicaa_posix_wcoredump	(int)))

(define-c-function WIFSTOPPED
  (int nausicaa_posix_wifstopped (int)))

(define-c-function WSTOPSIG
  (int nausicaa_posix_wstopsig (int)))


;;;; done

)

;;; end of file

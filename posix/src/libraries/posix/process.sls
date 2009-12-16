;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to process related POSIX functions
;;;Date: Fri Dec 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (posix process)
  (export

    ;; identification
    getpid		getppid

    ;; forking
    fork		fork-function

    ;; executing
    execv		execv-function
    execve		execve-function
    execvp		execvp-function
    system		system-function

    ;; waiting
    waitpid		waitpid-function
    waitpid/any		waitpid/any-my-group
    waitpid/group)
  (import (rnrs)
    (receive)
    (compensations)
    (posix helpers)
    (prefix (posix process primitives) primitive:))


;; process id

(define-parametrised getpid)
(define-parametrised getppid)

;; forking

(define-parametrised fork)

;; executing

(define-parametrised execv pathname args)
(define-parametrised execve pathname args envs)
(define-parametrised execvp pathname args)
(define-parametrised system command)

;; waiting

(define-primitive-parameter waitpid-function		primitive:waitpid)

(define (waitpid pid options)
  (unless (< 0 pid)
    (assertion-violation 'waitpid
      "expected strictly positive process id" pid))
  ((waitpid-function) pid options))

(define (waitpid/any options)
  ((waitpid-function) -1 options))

(define (waitpid/any-my-group options)
  ((waitpid-function) 0 options))

(define (waitpid/group gpid options)
  (unless (< 0 gpid)
    (assertion-violation 'waitpid/group
      "expected strictly positive process group id" gpid))
  ((waitpid-function) (- gpid) options))


;;;; done

)

;;; end of file

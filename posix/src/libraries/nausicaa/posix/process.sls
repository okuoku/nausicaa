;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: interface to process related POSIX functions
;;;Date: Fri Dec 19, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009, 2010, 2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (nausicaa posix process)
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
    waitpid/group
    (rename (primitive:integer-><process-term-status> integer-><process-term-status>))

    ctermid	ctermid-function
    setsid	setsid-function
    getsid	getsid-function
    getpgrp	getpgrp-function
    setpgid	setpgid-function
    tcgetpgrp	tcgetpgrp-function
    tcsetpgrp	tcsetpgrp-function
    tcgetsid	tcgetsid-function)
  (import (rnrs)
    (nausicaa language extensions)
    (nausicaa language compensations)
    (nausicaa posix helpers)
    (nausicaa posix typedefs)
    (prefix (nausicaa posix process primitives) primitive:))


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

(define-parametrised waitpid pid options)
(define-parametrised waitpid/any options)
(define-parametrised waitpid/any-my-group options)
(define-parametrised waitpid/group gpid options)

;; terminal identification

(define-parametrised ctermid)

;; process group

(define-parametrised setsid)
(define-parametrised getsid pid)
(define-parametrised getpgrp)
(define-parametrised setpgid pid pgid)

;; terminal access

(define-parametrised tcgetpgrp fd)
(define-parametrised tcsetpgrp fd pgid)
(define-parametrised tcgetsid fd)


;;;; done

)

;;; end of file

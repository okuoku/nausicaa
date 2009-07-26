;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: access to POSIX stub library for process functions
;;;Date: Fri Dec 19, 2008
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

(library (posix process stub)
  (export
    WIFEXITED?
    WEXITSTATUS?
    WIFSIGNALED?
    WTERMSIG?
    WCOREDUMP?
    WIFSTOPPED?
    WSTOPSIG?

    make-process-term-status make-process-term-status-record
    (rename (process-term-status-record? process-term-status?)))
  (import (r6rs)
    (uriel lang)
    (uriel foreign))

  (define stub-lib
    (let ((o (open-shared-object 'libnausicaa-posix.so)))
      (shared-object o)
      o))


;;;; code

(define-c-function platform-WIFEXITED
  (int nausicaa_posix_wifexited	(int)))

(define-c-function platform-WEXITSTATUS
  (int nausicaa_posix_wexitstatus (int)))

(define-c-function platform-WIFSIGNALED
  (int nausicaa_posix_wifsignaled (int)))

(define-c-function platform-WTERMSIG
  (int nausicaa_posix_wtermsig (int)))

(define-c-function platform-WCOREDUMP
  (int nausicaa_posix_wcoredump	(int)))

(define-c-function platform-WIFSTOPPED
  (int nausicaa_posix_wifstopped (int)))

(define-c-function platform-WSTOPSIG
  (int nausicaa_posix_wstopsig(int)))

(define-record-type process-term-status-record
  (fields (immutable WIFEXITED		WIFEXITED?)
	  (immutable WEXITSTATUS	WEXITSTATUS?)
	  (immutable WIFSIGNALED	WIFSIGNALED?)
	  (immutable WTERMSIG		WTERMSIG?)
	  (immutable WCOREDUMP		WCOREDUMP?)
	  (immutable WIFSTOPPED		WIFSTOPPED?)
	  (immutable WSTOPSIG		WSTOPSIG?)))

(define-syntax bool
  (syntax-rules ()
    ((_ ?form)
     (if ?form #t #f))))

(define (make-process-term-status status)
  (make-process-term-status-record
   (bool (platform-WIFEXITED status))
   (bool (platform-WEXITSTATUS status))
   (bool (platform-WIFSIGNALED status))
   (bool (platform-WTERMSIG status))
   (bool (platform-WCOREDUMP status))
   (bool (platform-WIFSTOPPED status))
   (bool (platform-WSTOPSIG status))))


;;;; done

)

;;; end of file

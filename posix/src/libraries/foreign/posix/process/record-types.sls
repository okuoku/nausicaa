;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: access to POSIX stub library for process functions
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


(library (foreign posix process record-types)
  (export

    <process-term-status>	<process-term-status-rtd>
    make-<process-term-status>	process-term-status->record
    <process-term-status>?

    WIFEXITED?			WEXITSTATUS?
    WIFSIGNALED?		WTERMSIG?
    WCOREDUMP?			WIFSTOPPED?
    WSTOPSIG?)
  (import (rnrs)
    (prefix (foreign posix process platform) platform:))


(define-record-type <process-term-status>
  (fields (immutable WIFEXITED		WIFEXITED?)
	  (immutable WEXITSTATUS	WEXITSTATUS?)
	  (immutable WIFSIGNALED	WIFSIGNALED?)
	  (immutable WTERMSIG		WTERMSIG?)
	  (immutable WCOREDUMP		WCOREDUMP?)
	  (immutable WIFSTOPPED		WIFSTOPPED?)
	  (immutable WSTOPSIG		WSTOPSIG?)))

(define <process-term-status-rtd>
  (record-type-descriptor <process-term-status>))

(define (process-term-status->record status)
  (let-syntax ((bool (syntax-rules ()
		       ((_ ?form)
			(if ?form #t #f)))))
    (make-<process-term-status>
     (bool (platform:WIFEXITED status))
     (bool (platform:WEXITSTATUS status))
     (bool (platform:WIFSIGNALED status))
     (bool (platform:WTERMSIG status))
     (bool (platform:WCOREDUMP status))
     (bool (platform:WIFSTOPPED status))
     (bool (platform:WSTOPSIG status)))))


;;;; done

)

;;; end of file

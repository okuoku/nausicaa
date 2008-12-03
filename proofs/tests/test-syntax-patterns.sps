;;;
;;;Part of: Nausicaa/Proofs
;;;Contents: tests for syntax objects
;;;Date: Fri Nov 28, 2008
;;;Time-stamp: <2008-11-28 13:41:40 marco>
;;;
;;;Abstract
;;;
;;;	This  test file  explores  the wonders  of  pattern matching  in
;;;	macros.
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

(import (rnrs)
  (uriel test)
  (uriel printing)
  (srfi parameters))

(check-set-mode! 'report-failed)



(parameterize ((testname 'patterns))

  (let ()
    (define-syntax doit
      (syntax-rules ()
	((_ ?elm0 ?elm ...)
	 (quote (?elm0 ?elm ...)))
	((_ . ?rest)
	 (quote error))))

    (check (doit 1)         => '(1))
    (check (doit 1 2 3)     => '(1 2 3))
    (check (doit)           => 'error))

  (let ()
    (define-syntax doit
      (syntax-rules ()
	((_ ?elm ...)
	 '(?elm ...))
	((_ . ?rest)
	 'error)))

    (check (doit 1)         => '(1))
    (check (doit 1 2 3)     => '(1 2 3))
    (check (doit)           => '())
    )

  (let ()

    (define-syntax doit
      (syntax-rules ()
	((_ . ?args)
	 '?args)))

    (check (doit 1)         => '(1))
    (check (doit 1 2 3)     => '(1 2 3))
    (check (doit 1 . 2)     => '(1 . 2)))

  (let ()
    (define-syntax doit
      (syntax-rules ()
	((_ ?arg ...)
	 '(?arg ...))))

    (check (doit 1)         => '(1))
    (check (doit 1 2 3)     => '(1 2 3))
    )

  (let ()

    (check
	(let ()
	  (define-syntax doit
	    (syntax-rules ()
	      ((doit _) 1)))

	  (list (doit '(a b c))
		(doit 2)
		(doit '(a (b (c)) d (e)))))
      => '(1 1 1))

    (check
	(let ()
	  (define-syntax doit
	    (syntax-rules ()
	      ((doit _ _) 1)))

	  (list (doit '(a b c) '(d e f))
		(doit 1 2)))
      => '(1 1))
    )

  )



;;;; done

(check-report)

;;; end of file

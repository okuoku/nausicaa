;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the environment variables functions
;;;Date: Sun Nov 30, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008-2011 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(import (nausicaa)
  (nausicaa checks)
  (nausicaa posix system)
  (nausicaa glibc system))

(check-set-mode! 'report-failed)
(display "*** testing POSIX and Glibc environment variables access\n")


(parametrise ((check-test-name 'setget))

  (check
      (getenv 'CIAO-CIAO-CIAO-MARE)
    => #f)

  (check
      (let ()
	(setenv 'CIAO "" #t)
	(getenv 'CIAO))
    => "")

  (check
      (let ()
	(setenv 'CIAO 'fusilli #t)
	(getenv 'CIAO))
    => "fusilli")

  (check
      (let ()
	(setenv 'CIAO 'fusilli #t)
	(setenv 'CIAO 'spaghetti #f)
	(getenv 'CIAO))
    => "fusilli")

  (check
      (let ()
	(setenv 'SALUT 'fusilli #t)
	(setenv 'SALUT 'fusilli #f)
	(getenv 'SALUT))
    => "fusilli")

  #t)


(parametrise ((check-test-name 'environ))

;;;  (pretty-print (environ))
;;;  (pretty-print (hashtable-keys (environ-table)))(newline)

  (check
      (let ((table (environ-table)))
	(hashtable-contains? table 'PATH))
    => #t)

  (check
      (hashtable-contains? (environ->table (table->environ (environ->table (environ)))) 'PATH)
    => #t)

  #t)


(parametrise ((check-test-name 'clear))

  (check
      (begin
	(setenv 'CIAO "ciao" #t)
	(unsetenv 'CIAO)
	(getenv 'CIAO))
    => #f)

  (check
      (begin
	(setenv 'CIAO "ciao" #t)
	(clearenv)
	(getenv 'CIAO))
    => #f)

  #t)


(parametrise ((check-test-name 'clear))

  (check
      (begin
	(clearenv)
	(putenv* 'CIAO "ciao")
	(getenv 'CIAO))
    => "ciao")

  (check
      (begin
	(clearenv)
	(putenv "CIAO=ciao")
	(getenv 'CIAO))
    => "ciao")

  (check
      (guard (E ((assertion-violation? E)
		 #t)
		(else (condition-message E)))
	(putenv 'CIAO "ciao"))
    => #t)


  #t)


;;;; done

(check-report)

;;; end of file

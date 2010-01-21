;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/POSIX
;;;Contents: tests for the system configuration inspection functions
;;;Date: Wed Dec  9, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009, 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (strings)
  (compensations)
  (deferred-exceptions)
  (pretty-print)
  (foreign errno)
  (posix sizeof)
  (posix typedefs)
  (prefix (posix system) posix:)
  (prefix (glibc system) glibc:)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing system inspection functions\n")


(parameterize ((check-test-name	'sysconf)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in sysconf" E))
    (lambda ()

      (check
	  (integer? (posix:sysconf _SC_ARG_MAX))
	=> #t)

      (check
	  (integer? (posix:pathconf "/bin/ls" _PC_NAME_MAX))
	=> #t)

      (check
	  (posix:confstr _CS_PATH)
	=> "/bin:/usr/bin")

      #t)))


(parameterize ((check-test-name	'names)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in names" E))
    (lambda ()

      (check
	  (posix:gethostname)
	=> "rapitore")

      (check
	  (posix:getdomainname)
	=> "(none)")

      (check
	  (let ((r (posix:uname)))
	    (<utsname>? r))
	=> #t)

      (check
	  (let ((r (posix:uname)))
	    (<utsname>-sysname r))
	=> "Linux")

      #t)))


(parameterize ((check-test-name	'ftab)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in fstab" E))
    (lambda ()

      (check
	  (begin
	    (glibc:setfsent)
	    (let ((tabs (let loop ((tabs '()))
			  (let ((t (glibc:getfsent)))
			    (if t
				(loop (cons t tabs))
			      (begin
				(glibc:endfsent)
				tabs))))))
;;;(pretty-print tabs)
	      (for-all <fstab>? tabs)))
	=> #t)

      (check
	  (begin
	    (glibc:setfsent)
	    (let ((tab (glibc:getfsspec "/dev/sda3")))
	      (glibc:endfsent)
;;;(pretty-print tab)(newline)
	      (<fstab>? tab)))
	=> #t)

      (check
	  (begin
	    (glibc:setfsent)
	    (let ((tab (glibc:getfsfile "/")))
	      (glibc:endfsent)
;;;(pretty-print tab)(newline)
	      (<fstab>? tab)))
	=> #t)

      #t)))


(parameterize ((check-test-name	'mtab)
	       (debugging	#f))

  (with-deferred-exceptions-handler
      (lambda (E)
	(debug-print-condition "deferred condition in mtab" E))
    (lambda ()

      (check
	  (let* ((stream (glibc:setmntent _PATH_MOUNTED "r"))
		 (tabs	 (let loop ((tabs '())
				    (entry (glibc:getmntent stream)))
			   (if entry
			       (loop (cons entry tabs) (glibc:getmntent stream))
			     (begin
			       (glibc:endmntent stream)
			       tabs)))))
;;;(pretty-print tabs)
	    (for-all <mntent>? tabs))
	=> #t)

      #t)))


;;;; done

(check-report)

;;; end of file
